#Read in csv
data <- read.csv("C:/Users/jacob/OneDrive - Central Michigan University/R folder/traffic_accidents.csv", 
                 header = TRUE, stringsAsFactors = FALSE)



# Reclassify lighting conditions BEFORE creating subsets
data$lighting_condition <- ifelse(data$lighting_condition %in% c("DAYLIGHT"), "Day",
                                  ifelse(data$lighting_condition %in% c("DARKNESS, LIGHTED ROAD"), "Street Lights",
                                         ifelse(data$lighting_condition %in% c("DUSK"), "Dusk",
                                                ifelse(data$lighting_condition %in% c("DARKNESS"), "No Street Lights",
                                                       ifelse(data$lighting_condition %in% c("DAWN"), "Dawn", "Other")))))


#create subsets
injured <- data[data$injuries_total > 0, ]
non_injured <- data[data$injuries_total == 0, ]

# Add a new column for accident type
injured$accident_type <- "Injury"
non_injured$accident_type <- "Non-Injury"

# Combine both datasets
combined_data <- rbind(injured, non_injured)

# Create a table of lighting conditions and accident type
lighting_counts <- as.data.frame(table(combined_data$lighting_condition, combined_data$accident_type))
# Rename columns
colnames(lighting_counts) <- c("Lighting", "Accident_Type", "Count")

# Ensure ordered factor levels
lighting_counts$Lighting <- factor(lighting_counts$Lighting, 
                                   levels = c("Day", "Street Lights", "No Street Lights", "Dusk", "Dawn", "Other"))
# Count total accidents (injury + non-injury) for each weather condition
total_counts <- data %>%
  group_by(lighting_condition) %>%
  summarise(Total = n(), .groups = "drop")

# Count injury accidents for each weather condition
injury_counts <- injured %>%
  group_by(lighting_condition) %>%
  summarise(Injuries = n(), .groups = "drop")

lighting_summary <- merge(total_counts, injury_counts, by = "lighting_condition", all.x = TRUE)
lighting_summary$Injuries[is.na(lighting_summary$Injuries)] <- 0
lighting_summary$Injury_Percentage <- (lighting_summary$Injuries / lighting_summary$Total) * 100

# Merge both counts
lighting_counts <- merge(lighting_counts, lighting_summary[, c("lighting_condition", "Injury_Percentage")], 
                         by.x = "Lighting", by.y = "lighting_condition", all.x = TRUE)
lighting_counts$label <- ifelse(lighting_counts$Accident_Type == "Injury", 
                                paste0(round(lighting_counts$Injury_Percentage, 1), "%"), 
                                NA)# Exclude "Day" from the dataset before plotting
lighting_counts_filtered <- lighting_counts[lighting_counts$Lighting != "Day", ]

# Plot the data
ggplot(lighting_counts_filtered, aes(x = Lighting, y = Count, fill = Accident_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = label), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3, na.rm = TRUE) +
  scale_y_continuous(breaks = seq(0, max(lighting_counts_filtered$Count, na.rm = TRUE), by = 5000)) +  # More ticks
  labs(
    title = "Comparison of Injury vs Non-Injury Accidents by Lighting Condition (Excluding Daylight)",
    x = "Lighting Condition",
    y = "Number of Accidents"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(family = "Times", face = "bold", size = 14, color = "black"),  # X-axis
        axis.title.y = element_text(family = "Times", face = "bold", size = 14, color = "black"),  # Y-axis
        plot.title = element_text(family = "Arial", face = "bold", size = 10, color = "black", hjust = 0.25))  # Center title
