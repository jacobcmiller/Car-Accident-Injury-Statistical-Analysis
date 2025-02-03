#Read in csv
data <- read.csv("C:/Users/jacob/OneDrive - Central Michigan University/R folder/traffic_accidents.csv", 
                 header = TRUE, stringsAsFactors = FALSE)

# Convert weather conditions to lowercase for consistency
data$weather_condition <- tolower(data$weather_condition)

# Reclassify weather conditions BEFORE creating subsets
data$weather_condition <- ifelse(data$weather_condition %in% c("snow", "blowing snow"), "Snow",
                                 ifelse(data$weather_condition %in% c("rain", "freezing rain/drizzle", "sleet/hail"), "Rain",
                                        ifelse(data$weather_condition %in% c("cloudy/overcast"), "Cloudy",
                                               ifelse(data$weather_condition %in% c("clear"), "Clear",
                                                      ifelse(data$weather_condition %in% c("fog/smoke/haze"), "Foggy", "Other")))))


#create subsets
injured <- data[data$injuries_total > 0, ]
non_injured <- data[data$injuries_total == 0, ]

# Add a new column for accident type
injured$accident_type <- "Injury"
non_injured$accident_type <- "Non-Injury"

# Combine both datasets
combined_data <- rbind(injured, non_injured)

# Create a table of weather conditions and accident type
weather_counts <- as.data.frame(table(combined_data$weather_condition, combined_data$accident_type))

# Rename columns
colnames(weather_counts) <- c("Weather", "Accident_Type", "Count")

# Ensure ordered factor levels
weather_counts$Weather <- factor(weather_counts$Weather, 
                                 levels = c("Clear", "Rain", "Cloudy", "Snow", "Foggy", "Other"))

# Count total accidents (injury + non-injury) for each weather condition
total_counts <- data %>%
  group_by(weather_condition) %>%
  summarise(Total = n(), .groups = "drop")

# Count injury accidents for each weather condition
injury_counts <- injured %>%
  group_by(weather_condition) %>%
  summarise(Injuries = n(), .groups = "drop")

weather_summary$Injury_Percentage <- (weather_summary$Injuries / weather_summary$Total) * 100

# Merge both counts
weather_counts <- merge(weather_counts, weather_summary[, c("weather_condition", "Injury_Percentage")], 
                        by.x = "Weather", by.y = "weather_condition", all.x = TRUE)
weather_counts$label <- ifelse(weather_counts$Accident_Type == "Injury", 
                               paste0(round(weather_counts$Injury_Percentage, 1), "%"), 
                               NA)

# Fill missing injury values with 0
weather_summary$Injuries[is.na(weather_summary$Injuries)] <- 0

# Filter out "Clear" weather conditions
weather_counts_filtered <- weather_counts[weather_counts$Weather != "Clear", ]
weather_counts_filtered <- weather_counts_filtered[weather_counts_filtered$Weather != "Other", ]

# Plot without "Clear" weather conditions
ggplot(weather_counts_filtered, aes(x = Weather, y = Count, fill = Accident_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = label), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3, na.rm = TRUE) +
  scale_y_continuous(breaks = seq(0, max(weather_counts_filtered$Count, na.rm = TRUE), by = 1000)) +  # More ticks
  labs(
    title = "Comparison of Injury vs Non-Injury Accidents by Weather Condition (Excluding Clear)",
    x = "Weather Condition",
    y = "Number of Accidents"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(family = "Times", face = "bold", size = 14, color = "black"),  # X-axis
        axis.title.y = element_text(family = "Times", face = "bold", size = 14, color = "black"),  # Y-axis
        plot.title = element_text(family = "Arial", face = "bold", size = 10, color = "black", hjust = 0.25))  # Center title
