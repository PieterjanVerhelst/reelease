# Analyse departure direction
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be


library(tidyverse)
library(lubridate)


# Load data obtained by Martin
data <- read_csv("./data/raw/departure_directions.csv")
data$tag_id <- factor(data$tag_id)


# Load eel metadata
eel <- read_csv("./data/raw/eel_metadata.csv")

eel <- eel %>%
  select(transmitter, length_mm, weight_g, dst, release_time) %>%
  rename(tag_id = transmitter)

# Merge eel data to detection data
data <- left_join(data, eel, by = "tag_id")


# Add year
data$release_time <- dmy_hm(data$release_time)
data$year <- year(data$release_time)
data$year <- factor(data$year)


# Paired t-test
# independent 2-group t-test
# t.test(y~x) # where y is numeric and x is a binary factor 
t.test(data$departure_direction~data$dst, var.equal = TRUE)

# Assumptions:
# 1. 2 independent groups
# N and SW are independent (each direction is attributed to a different animal)

# 2. Normality
qqnorm(data$departure_direction, pch = 1, frame = TRUE)
qqline(data$departure_direction, col = "steelblue", lwd = 2)

shapiro.test(data$departure_direction)
# The p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.

# 3. Homogeneity of variances
# F-test 
# The F-test is used to assess whether the variances of two populations (A and B) are equal. The test requires normality (in case no normality, apply Fligner-Killeen’s test)
# https://www.datanovia.com/en/lessons/homogeneity-of-variance-test-in-r/
var.test(departure_direction ~ dst, data = data)
# When p > 0.05, there is no significant difference between the two variances.


# Mann-Whitney U test as non-parametric alternative for the independent 2-group t-test
# https://www.statmethods.net/stats/nonparametric.html
wilcox.test(departure_direction ~ dst, data = data)


