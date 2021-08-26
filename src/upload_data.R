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
  rename("date_time" = "Date and Time (UTC)",
         "receiver_id" = "Receiver",
         "tag_id" = "Transmitter",
         "station_name" = "Station Name")

vemco_data <- select(vemco_data, date_time, tag_id, station_name, receiver_id)
vemco_data$date_time <- ymd_hms(vemco_data$date_time)


# 2. Upload Thelma data ####
thelma_data <- read_csv("./data/raw/thelma/johnring2020thelma.csv")

# Rename and remove columns
thelma_data <- thelma_data %>%
  rename("date_time" = "Date and Time (UTC)",
         "receiver_id" = "Receiver",
         "tag_id" = "ID",
         "protocol" = "Protocol")

thelma_data$station_name <- "Johnring"
thelma_data <- select(thelma_data, date_time, protocol, tag_id, station_name, receiver_id)

# Filter relevant transmitter IDs
thelma_data <- filter(thelma_data, protocol == "R64K-69kHz")

# Add prefix to transmitter ID and remove protocol column
thelma_data$tag_id = paste0('A69-1303-', thelma_data$tag_id)
thelma_data$protocol <- NULL
thelma_data$date_time <- ymd_hms(thelma_data$date_time)


# 3. Bind Vemco and Thelma datasets ####
data <- rbind(vemco_data, thelma_data)


# 4. Adjust column types ####
data$tag_id <- factor(data$tag_id)
data$station_name <- factor(data$station_name)
data$receiver_id <- factor(data$receiver_id)


# 5. Add station coordinates ####
network <- read_csv("./data/raw/receivernetwork.csv")
network$station_name <- factor(network$station_name)
network <- network %>%
  rename("deploy_latitude" = "latitude",
         "deploy_longitude" = "longitude")

data <- left_join(data, network, by = "station_name")


# 6. Add moment of release ####
eel <- read_csv("./data/raw/eel_metadata.csv")
eel$receiver_id <- "none"
eel <- eel %>%
  select(release_time, transmitter, station_name, receiver_id, release_latitude, release_longitude) %>%
  rename(date_time = release_time,
         tag_id = transmitter,
         deploy_latitude = release_latitude,
         deploy_longitude = release_longitude)

# Set columns
eel$date_time <- dmy_hm(eel$date_time)
eel$tag_id <- factor(eel$tag_id)
eel$station_name <- factor(eel$station_name)
eel$receiver_id <- factor(eel$receiver_id)

# Bind release to dataset
data <- rbind(eel, data)


# 7. Select data from this eel study ####
data <- data[data$tag_id %in% eel$tag_id,]
data$animal_project_code <- "Reelease"
data$animal_project_code <- factor(data$animal_project_code)

# 8. Remove redundant data ####
rm(eel)
rm(network)
rm(thelma_data)
rm(vemco_data)


