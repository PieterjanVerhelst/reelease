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


# Summarise max tracking time ####
residence <- data2 %>%
  group_by(tag_id) %>%
  summarise(total_time = max(date_time) - min(date_time))

residence$total_time <- as.numeric(residence$total_time/3600)


# Add column with dst ####
# Load eel metadata
eel <- read_csv("./data/raw/eel_metadata.csv")
eel <- eel %>%
  select(transmitter, length_mm, weight_g, dst) %>%
  rename(tag_id = transmitter)

# Merge eel data to detection data
residence <- left_join(residence, eel, by = "tag_id")
residence$dst <- factor(residence$dst)


# Remove 6 recaptured eels from analysis
residence <- residence[!(residence$tag_id == "A69-9006-5421" |
                           residence$tag_id == "A69-9006-5426" |
                           residence$tag_id == "A69-9006-5429" |
                           residence$tag_id == "A69-9006-5430" |
                           residence$tag_id == "A69-9006-5433" |
                           residence$tag_id == "A69-9006-5440" ),]


# Calculate summaries ####
aggregate(residence$total_time, list(residence$dst), median)

# Create simple boxplot of speed
boxplot(total_time~dst, residence,
        xlab = "DST",
        ylab = "Residence time (h)",
        ylim=c(0, 30), cex.lab=1.25, cex.axis=1.25)

# Create elaborated boxplot with number of eels per class
# make a named list for the location of the number of eels
eel_per_class <- residence %>% group_by(dst) %>% 
  summarise(n_eels = n_distinct(tag_id))
eels_per_class_list <- rep(110, nrow(eel_per_class))
names(eels_per_class_list) <- as.vector(eel_per_class$dst)
# create ggplot (cfr. styling earlier plot)
fig_residence <- ggplot(residence, aes(x = dst,
                                                 y = total_time)) +
  geom_boxplot() +
  geom_point(alpha = 1.5, size = 3) +
  scale_y_continuous(breaks = seq(0, 110, by = 10), limits=c(0,110)) +
  theme_minimal() +
  ylab("Residence time (h)") +
  geom_text(data = data.frame(),
            aes(x = names(eels_per_class_list),
                y = eels_per_class_list,
                label = as.character(eel_per_class$n_eels)),
            col = 'black', size = 6) +
  scale_x_discrete(limits=c("0","1")) +    # Changes oreder of plots
  xlab("DST") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
fig_residence




# independent 2-group t-test
# t.test(y~x) # where y is numeric and x is a binary factor 
t.test(residence$total_time~residence$dst, var.equal = TRUE)

# Assumptions:
# 1. 2 independent groups
# N and SW are independent (each direction is attributed to a different animal)

# 2. Normality
qqnorm(residence$total_time, pch = 1, frame = TRUE)
qqline(residence$total_time, col = "steelblue", lwd = 2)

shapiro.test(residence$total_time)
# The p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.

# 3. Homogeneity of variances
# F-test 
# The F-test is used to assess whether the variances of two populations (A and B) are equal. The test requires normality (in case no normality, apply Fligner-Killeen’s test)
# https://www.datanovia.com/en/lessons/homogeneity-of-variance-test-in-r/
var.test(total_time ~ dst, data = residence)
# When p > 0.05, there is no significant difference between the two variances.


# Mann-Whitney U test as non-parametric alternative for the independent 2-group t-test
# https://www.statmethods.net/stats/nonparametric.html
wilcox.test(total_time ~ dst, data = residence)








