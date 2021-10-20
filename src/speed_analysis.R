# Migration speed analysis in relation to external tagging
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be



# Remove 6 recaptured eels from analysis
speed <- speed[!(speed$tag_id == "A69-9006-5421" |
                    speed$tag_id == "A69-9006-5426" |
                    speed$tag_id == "A69-9006-5429" |
                    speed$tag_id == "A69-9006-5430" |
                    speed$tag_id == "A69-9006-5433" |
                    speed$tag_id == "A69-9006-5440" ),]


# Check distances
check <- select(speed, tag_id, swimdistance_m, totaldistance_m, distance_to_source_m)

# Summarise max distances
max <- speed %>%
  group_by(tag_id) %>%
  summarise(distance_to_source_m = max(distance_to_source_m))

# Select eels that migrated > 10000
eel_selection <- filter(max, distance_to_source_m > 10000)

# Select those eels in speed, eel and max data set
speed <- speed[speed$tag_id %in% eel_selection$tag_id,]
eel <- eel[eel$tag_id %in% eel_selection$tag_id,]
table(eel$dst)
max_distance <- max[max$tag_id %in% eel_selection$tag_id,]

# Calculate summaries
aggregate(speed$speed_m_s, list(speed$dst), mean)

# Create boxplot of speed
boxplot(speed_m_s~dst, speed,
        xlab = "DST",
        ylab = "Migration speed (m/s)",
        ylim=c(0, 5), cex.lab=1.25, cex.axis=1.25)

# Summarise max tracking time
max_time <- speed %>%
  group_by(tag_id) %>%
  summarise(total_time = max(departure) - min(arrival))


# Merge max_distance with max_time
total_tracking <- left_join(max_distance, max_time, by = "tag_id")

# Merge eel characteristics
total_tracking <- left_join(total_tracking, eel, by = "tag_id")
total_tracking$dst <- factor(total_tracking$dst)

# Calculate total tracking speed
total_tracking$distance_to_source_km <- total_tracking$distance_to_source_m / 1000
total_tracking$total_time <- as.numeric(total_tracking$total_time)
total_tracking$total_speed_km_day <- total_tracking$distance_to_source_km / total_tracking$total_time

# Calculate summaries
aggregate(total_tracking$total_speed_km_day, list(total_tracking$dst), mean)

# Create simple boxplot of speed
boxplot(total_speed_km_day~dst, total_tracking,
        xlab = "DST",
        ylab = "Migration speed (km/day)",
        ylim=c(0, 30), cex.lab=1.25, cex.axis=1.25)

# Create elaborated boxplot with number of eels per class
# make a named list for the location of the number of eels
eel_per_class <- total_tracking %>% group_by(dst) %>% 
  summarise(n_eels = n_distinct(tag_id))
eels_per_class_list <- rep(30, nrow(eel_per_class))
names(eels_per_class_list) <- as.vector(eel_per_class$dst)
# create ggplot (cfr. styling earlier plot)
fig_migrationspeed <- ggplot(total_tracking, aes(x = dst,
                                            y = total_speed_km_day)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  theme_minimal() +
  ylab("Migration speed (km/day)") +
  geom_text(data = data.frame(),
            aes(x = names(eels_per_class_list),
                y = eels_per_class_list,
                label = as.character(eel_per_class$n_eels)),
            col = 'black', size = 4) +
  scale_x_discrete(limits=c("0","1")) +    # Changes oreder of plots
  xlab("DST") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
fig_migrationspeed





# independent 2-group t-test
# t.test(y~x) # where y is numeric and x is a binary factor 
t.test(total_tracking$total_speed_km_day~total_tracking$dst, var.equal = TRUE)

# Assumptions:
# 1. 2 independent groups
# N and SW are independent (each direction is attributed to a different animal)

# 2. Normality
qqnorm(total_tracking$total_speed_km_day, pch = 1, frame = TRUE)
qqline(total_tracking$total_speed_km_day, col = "steelblue", lwd = 2)

shapiro.test(total_tracking$total_speed_km_day)
# The p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.

# 3. Homogeneity of variances
# F-test 
# The F-test is used to assess whether the variances of two populations (A and B) are equal. The test requires normality (in case no normality, apply Fligner-Killeen’s test)
# https://www.datanovia.com/en/lessons/homogeneity-of-variance-test-in-r/
var.test(total_speed_km_day ~ dst, data = total_tracking)
# When p > 0.05, there is no significant difference between the two variances.


# Mann-Whitney U test as non-parametric alternative for the independent 2-group t-test
# https://www.statmethods.net/stats/nonparametric.html
wilcox.test(total_speed_km_day ~ dst, data = total_tracking)





