# Step 1
# Load Data

# Step 2: Clean Data
# 1. Add space between zipcode and state
# 2. Determine what to do with blank address entries
# 3. Make sure each booking ID/inmate ID only has one row 

# Load required package
library(dplyr)
library(tidycensus)
library(tidygeocoder)
library(tmaptools)
library(ggplot2)
library(sf)
library(gridExtra)

setwd("/Users/cgernon/GitHub/code-review/Mazen Final Project/")
# Register Census API key
census_api_key <- "4ef39bbbf713f3d7e5f93edc1fc144a42d8b8377"
census_api_key <- Sys.getenv("4ef39bbbf713f3d7e5f93edc1fc144a42d8b8377")
options(tigris_use_cache = TRUE)  # Enable caching for faster data retrieval

# Load pretrial data
pretrial_data <- read.csv("./Data/Vera FOIL 2022.csv")

# Clean the address data using mutate
pretrial_data <- pretrial_data %>%
  mutate(
    # Add space between state and zip code in the address column
    Address = gsub("([A-Za-z]+)(\\d{5})", "\\1 \\2", Address)
  )



# Remove rows with blank addresses, "Homeless" addresses, and select desired columns
pretrial_data <- pretrial_data %>%
  filter(grepl("\\S", Address) & Address != "Homeless") %>%
  
  select(-Booking.ID.number)  # Remove the Booking.ID.number column

pretrial_data$Address <- gsub("APT 11I", "", pretrial_data$Address)
pretrial_data$Address <- gsub("APT 11I", "", pretrial_data$Address)
pretrial_data$Address <- gsub("APT\\s*\\d+", "", pretrial_data$Address)
pretrial_data$Address<- trimws(pretrial_data$Address)
pretrial_data$Address <- gsub("CTY RT", "County Route", pretrial_data$Address)


# Geocode addresses to lat/long using tidygeocoder

#test<-head(pretrial_data)
df <- pretrial_data[1:20, ]


a<-geocode_OSM(df$Address,
               as.data.frame = T,
               geometry = "point")
### census data
income_data_2020 <- get_acs(
  geography = "tract",
  variables = "B19013_001", # Median Household Income
  year = 2020,
  state = "NY",
  county = "Orange",
  geometry = TRUE,
  output = "wide",
  key = "4ef39bbbf713f3d7e5f93edc1fc144a42d8b8377"
)

ggplot()+
geom_sf(data = income_data_2020)+ 
  geom_point(aes(lon, lat), 
              data = a, na.rm=T)
