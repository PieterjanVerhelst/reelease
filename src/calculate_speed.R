# Calculate migration speeds between consecutive detection stations
# by Pieterjan Verhelst
# pieterjan.verhelst@inbo.be


# Packages
library(tidyverse)
library(magrittr)
library(lubridate)
library(actel)
#browseVignettes("actel")

# Source functions
source("./src/calculate_speed_function.R")

# Load data
residency <- read_csv("./data/interim/residency.csv")
residency$...1 <- NULL

# Load distance matrix
# Make sure the first column is not containing the station names
distance_matrix <- read.csv("./data/external/distancematrix_reelease.csv",  row.names = 1, check.names=FALSE)

# Calculate speed without taking into account different tag_id
#speed <- movementSpeeds(residency, "last to first", distance_matrix)

# Turn dataset into list per tag_id
residency_list <- split(residency , f = residency$tag_id)
#sapply(residency_list, function(x) max(x$detections))

# Calculate speed per tag_id
speed_list <- lapply(residency_list, function(x) movementSpeeds(x, "last to first", distance_matrix))
#speed_list[[1]]

# Turn lists back into dataframe
#speed <- do.call(rbind.data.frame, speed)
speed <- plyr::ldply (speed_list, data.frame)
speed$.id <- NULL


# Calculate total swim distance per tag_id
speed <- speed %>% 
  group_by(tag_id) %>%
  mutate(totaldistance_m=cumsum(coalesce(swimdistance_m, 0)) + swimdistance_m*0)

# Set row with release station at 0 total distance
speed <- speed %>% 
  #group_by(id) %>% 
  mutate(totaldistance_m = ifelse(row_number() == 1 | all(is.na(totaldistance_m)), 0, totaldistance_m))

# Replace the remaining NA values with previous non-NA value
speed$totaldistance_m <- zoo::na.locf(speed$totaldistance_m)

               

# Write csv
write.csv(speed, "./data/interim/speed.csv")


