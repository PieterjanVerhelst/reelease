# Upload detection data from the raw folder
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be


library(tidyverse)
library(lubridate)

# 1. Upload Vemco/InnovaSea data ####
vemco_data <-  list.files(path = "./data/raw/vemco/",
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) 

# Rename and remove columns
vemco_data <- vemco_data %>%
  rename("datetime" = "Date and Time (UTC)",
         "receiver" = "Receiver",
         "transmitter" = "Transmitter",
         "station_name" = "Station Name")

vemco_data <- select(vemco_data, datetime, transmitter, station_name, receiver)
vemco_data$datetime <- ymd_hms(vemco_data$datetime)


# 2. Upload Thelma data ####
thelma_data <- read_csv("./data/raw/thelma/johnring2020thelma.csv")

# Rename and remove columns
thelma_data <- thelma_data %>%
  rename("datetime" = "Date and Time (UTC)",
         "receiver" = "Receiver",
         "transmitter" = "ID",
         "protocol" = "Protocol")

thelma_data$station_name <- "Johnring"
thelma_data <- select(thelma_data, datetime, protocol, transmitter, station_name, receiver)

# Filter relevant transmitter IDs
thelma_data <- filter(thelma_data, protocol == "R64K-69kHz")

# Add prefix to transmitter ID and remove protocol column
thelma_data$transmitter = paste0('A69-1303-', thelma_data$transmitter)
thelma_data$protocol <- NULL
thelma_data$datetime <- ymd_hms(thelma_data$datetime)


# 3. Bind Vemco and Thelma datasets ####
data <- rbind(vemco_data, thelma_data)


# 4. Adjust column types ####
data$transmitter <- factor(data$transmitter)
data$station_name <- factor(data$station_name)
data$receiver <- factor(data$receiver)


# 5. Add station coordinates ####
network <- read_csv("./data/raw/receivernetwork.csv")
network$station_name <- factor(network$station_name)

data <- left_join(data, network, by = "station_name")


# 6. Add moment of release ####
eel <- read_csv("./data/raw/eel_metadata.csv")
eel$receiver <- "none"
eel <- eel %>%
  select(release_time, transmitter, station_name, receiver, release_latitude, release_longitude) %>%
  rename(datetime = release_time,
         latitude = release_latitude,
         longitude = release_longitude)

# Set columns
eel$datetime <- dmy_hm(eel$datetime)
eel$transmitter <- factor(eel$transmitter)
eel$station_name <- factor(eel$station_name)
eel$receiver <- factor(eel$receiver)

# Bind release to dataset
data <- rbind(eel, data)




