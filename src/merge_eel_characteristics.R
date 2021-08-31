# Attach eel metadata to detection dataset
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be

# Load data
speed <- read_csv("./data/interim/speed.csv")
speed[1] <- NULL

# Load eel metadata
eel <- read_csv("./data/raw/eel_metadata.csv")

eel <- eel %>%
  select(transmitter, length_mm, weight_g, dst) %>%
  rename(tag_id = transmitter)

# Merge eel data to detection data
speed <- left_join(speed, eel, by = "tag_id")


