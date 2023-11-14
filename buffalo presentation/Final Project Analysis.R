# Load required packages
library(dplyr)
library(tidycensus)
library(ggplot2)
library(sf)

# Register Census API key and set caching options
options(tigris_use_cache = TRUE)

# Load Data
`Vera FOIL 2022` <- read.csv("C:/Users/malsafi/OneDrive - Vera Institute of Justice/Documents/GitHub/code-review/Mazen Final Project/Data/Vera FOIL 2022.csv")
addresses <- read.csv("C:/Users/malsafi/OneDrive - Vera Institute of Justice/Documents/GitHub/code-review/Mazen Final Project/addresses.csv")

# Data Cleaning and Transformation
joined_data <- left_join(`Vera FOIL 2022`, addresses, by = c("Address", "Inmate.ID")) %>%
  
  # Add space between state and zip code in the address column and remove extra spaces
  mutate(Address = gsub("([A-Za-z]+)(\\d{5})", "\\1 \\2", Address),
         Address = gsub("\\s+", " ", Address)) %>%
  
  # Filter data
  filter(grepl("\\S", Address) & Address != "Homeless" & grepl("New York", Address, ignore.case = TRUE)) %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  
  # Select necessary columns and remove duplicates
  select(-Booking.ID.number) %>%
  distinct(Latitude, Longitude, Address, Inmate.ID, .keep_all = TRUE)

# Census data for Median Household Income
income_data_2020 <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  year = 2020,
  state = "NY",
  county = "Orange",
  geometry = TRUE,
  output = "wide",
  key = "4ef39bbbf713f3d7e5f93edc1fc144a42d8b8377"
)

# Plotting
ggplot() +
  geom_sf(data = income_data_2020, aes(fill = B19013_001E)) +
  geom_point(data = joined_data, aes(x = Longitude, y = Latitude), color = "red", size = 1, alpha = 0.25) +  # Adjusted alpha for increased transparency
  scale_fill_gradient(name = "Median Income", low = "lightblue", high = "darkblue") +
  labs(title = "Median Household Income Map", subtitle = "Orange County, NY 2020") +
  theme_minimal() +
  coord_sf(xlim = c(-74.895, -74.0), ylim = c(41.0, 41.7))  # Adjusted for a slightly more zoomed-out view
