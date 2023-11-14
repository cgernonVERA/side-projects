# Load necessary libraries
library(dplyr)
library(tidygeocoder)
library(censusxy)

# Register Census API key
census_api_key <- "4ef39bbbf713f3d7e5f93edc1fc144a42d8b8377"
census_api_key <- Sys.getenv("4ef39bbbf713f3d7e5f93edc1fc144a42d8b8377")
options(tigris_use_cache = TRUE)  # Enable caching for faster data retrieval

# link to documentation https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.pdf

# link to website where we can do it online https://geocoding.geo.census.gov/geocoder/locations/addressbatch?form

#sample dataset they provide that the address needs to be in (it's only one line): 
sample<- read.csv("./sample-addresses.csv", header = F)

test<-cxy_geocode(sample, street = 'V2', city = 'V3', state = 'V4', zip = 'V5',
            return = 'locations', class = 'dataframe', output = 'simple')
  

# Read in the data
data <- read.csv("./Data/erie_data.csv")#select(Inmate.ID, Address)

clean_homeless<-data%>%
  filter(grepl("homeless|null", .$Address, ignore.case = TRUE) | is.na(Address))%>%
  ChargeIdentifier(., "Charge.Information", group_var = sym("Inmate.ID"), only_PL_VTL = T)%>%
  filter(top_charge == T)%>%
  group_by(severity)%>%
  dplyr::summarise(N = n())

clean_homeless%>%
  mutate(booking.clean = mdy(Booking.Date),
         release.clean = mdy(Release.Date),
         los =release.clean- booking.clean,
         year = year(booking.clean),
         month = month(booking.clean),
         date = ymd(paste0(year, "-", month, "-", "01")))%>%
  filter(!is.na(los))%>%
  dplyr::summarize(avg_day = mean(los))
  

  

number<- data%>%
  ChargeIdentifier(., "Charge.Information", group_var = sym("Inmate.ID"), only_PL_VTL = T)%>%
  filter(top_charge == T)

number%>%
  mutate(booking.clean = mdy(Booking.Date),
         release.clean = mdy(Release.Date),
         los =release.clean- booking.clean,
         year = year(booking.clean),
         month = month(booking.clean),
         date = ymd(paste0(year, "-", month, "-", "01")))%>%
  filter(!is.na(los))%>%
  dplyr::summarize(avg_day = mean(los))

test<-data%>%
  #head(100)%>%
  ChargeIdentifier(., "Charge.Information", group_var = sym("Inmate.ID"), only_PL_VTL = T)%>%
  filter(top_charge == T)%>%
  mutate(address = trimws(Address))%>%
  filter(!is.na(Address) & Address != "" & Address != "NULL"&
           !grepl("Homeless", Address, ignore.case = TRUE))%>%
  select(address, Inmate.ID)%>%
  mutate(state = str_extract(address, "[A-Za-z]+(?=\\d{5})"),
         state = ifelse(state == "YORK", "NEW YORK", state),
         city = str_extract(address, "\\b\\w+\\b(?=\\s+NEW YORK)"),
         city = ifelse(grepl("buf", city, ignore.case = T), "BUFFALO", city),
         zip_code = str_extract(address, "\\d{5}$"),
         street = str_extract(address, ".*(?=\\sBuffalo)|.*(?=\\sBUFFALO)"))%>%
  filter(city == "BUFFALO")

test2<-cxy_geocode(test, street = 'street', city = 'city', state = 'state', zip = 'zip_code',
                  return = 'locations', class = 'dataframe', output = 'simple')

cleaned<-test2%>%
  filter(!is.na(cxy_lon))%>%
  filter(state =="NEW YORK")%>%
  filter(grepl("buffalo", city, ignore.case = T))

write.csv(cleaned, "buffalo.csv")
variable = c( "B19013_001","B01003_001")
# Census data for Median Household Income
income_data_2020 <- get_acs(
  geography = "block group",
  variables = variable,
  year = 2020,
  state = "NY",
  county = "Erie",
  geometry = TRUE,
  output = "wide",
  key = "4ef39bbbf713f3d7e5f93edc1fc144a42d8b8377"
)

st_write(income_data_2020, "buffalo_income2.geojson", append = FALSE)
v17 <- load_variables(2021, "acs5", cache = TRUE)


# Plotting
ggplot() +
  geom_sf(data = income_data_2020, aes(fill = B19013_001E)) +
  geom_point(data = cleaned, aes(x = cxy_lon, y = cxy_lat), color = "red", size = 1, alpha = 0.05) +  # Adjusted alpha for increased transparency
  scale_fill_gradient(name = "Median Income", low = "lightblue", high = "darkblue") +
  labs(title = "Median Household Income Map", subtitle = "Orange County, NY 2020") +
  theme_minimal()+
  coord_sf(xlim = c(-79.1, -78.5), ylim = c(42.4, 43.1))  # Adjusted for a slightly more zoomed-out view


# Cleaning Addresses for Geocoding:
# Filtering out non-New York addresses and those labeled "Homeless"
cleaned_data <- `Vera FOIL 2022` %>%
  filter(!is.na(Address) & Address != "" & 
           !grepl("Homeless", Address, ignore.case = TRUE) &
           grepl("New York", Address, ignore.case = TRUE)) %>%
  # Removing apartment numbers and similar references
  mutate(Address = gsub("\\bApt\\.?\\s?#?[0-9]+", "", Address)) %>%
  # Trim spaces and ensure format consistency, especially for the ZIP code
  mutate(Address = trimws(Address)) %>%  
  mutate(Address = ifelse(grepl("New York[0-9]{5}$", Address),
                          sub("([0-9]{5})$", " \\1", Address),
                          Address))

# Subset the data for geocoding testing
subset_data <- cleaned_data %>%
  sample_n(50)

# Next steps 
# 1. Clean addresses so apt numbers and extra characters are taken out
# 2. The street, city, and zip code are in different columns
# 3. There are only addresses in new york state
# 4. Addresses are valid ( there are instances where there is ave when it should be st. )

###############################################################################
#Another package we could use
###############################################################################

# You can also geocoding using tidygeocoder through OSM
# This step involves sending the cleaned addresses to a geocoding service (in this case, OpenStreetMap) to retrieve their latitude and longitude.
subset_data <- subset_data %>%
  geocode(Address, method = "osm", lat = "Latitude", lon = "Longitude")

# It's important to note that geocoding through OSM can sometimes result in errors or no matches. Errors can be due to:
# 1. Invalid Addresses: The address might not exist or might be too vague.
# 2. API Limits: Some geocoding services limit the number of requests within a certain time frame.

################################################################
# Things to consider:
# See if Vera can provide the research departments with Google API keys
# Google Maps is one of the most comprehensive and accurate geocoding services available
# API limits are more generous than other geocoding services
# Google's API can infer and correct minor mistakes in addresses, 
# leading to successful geocoding even if the input address has typos 
# or is incomplete (Very important for our case)