# Analyse release behaviour
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be


# Source functions ####
source("./src/upload_data.R")


# Filter data with release site stations ####
data <- data %>%
  filter(station_name == "Johnring"|
         station_name == "Releasesite1"|
         station_name == "Releasesite2"|
         station_name == "Releasesite3"|
         station_name == "Releasesite4"|
         station_name == "Releasesite5")


# Filter last row ####
data2 <- data %>%
  group_by(tag_id) %>%
  arrange(desc(date_time)) %>%
  filter(row_number()==1)

# Add moment of release ####
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
eel$date_time <- eel$date_time - 3600   # correct for winter time
eel$tag_id <- factor(eel$tag_id)
eel$station_name <- factor(eel$station_name)
eel$receiver_id <- factor(eel$receiver_id)
eel$animal_project_code <- "Reelease"

# Bind release to dataset
data2 <- rbind(eel, data2)



