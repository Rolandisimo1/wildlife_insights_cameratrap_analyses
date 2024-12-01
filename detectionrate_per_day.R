library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(ggpubr)

# Load the datasets
deployments <- read.csv("deployments.csv")  # Replace with the correct file path
sequences <- read.csv("sequences.csv")  # Replace with the correct file path

# Convert the start and end dates to Date format with time included
deployments$start_date <- as.POSIXct(deployments$start_date, format = "%Y-%m-%d %H:%M:%S")
deployments$end_date <- as.POSIXct(deployments$end_date, format = "%Y-%m-%d %H:%M:%S")

# Create a detection history for each camera for each day of operation
detection_history <- deployments %>%
  rowwise() %>%
  mutate(day_number = list(0:(as.numeric(difftime(end_date, start_date, units = "days"))))) %>%
  unnest(day_number) %>%
  mutate(date = as.Date(start_date + days(day_number))) %>%
  select(subproject_name, deployment_id, date, day_number)

# Join sequences with deployments to get subproject_name, ensuring a unique join
deployments_unique <- deployments %>% distinct(deployment_id, subproject_name)
sequences_joined <- sequences %>%
  left_join(deployments_unique, by = "deployment_id")

# Filter sequences to only include Class Mammalia, where genus and species are both present, and the common name is not NA
sequences_filtered <- sequences_joined %>%
  filter(class == "Mammalia" & !is.na(genus) & !is.na(species) & !is.na(common_name) & species != "")

# Convert the detection timestamp to Date format using the 'start_time' column
sequences_filtered <- sequences_filtered %>%
  mutate(detection_date = as.Date(as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S")))

# Count the number of sequences detected on each day for each species by each camera
detections <- sequences_filtered %>%
  select(deployment_id, detection_date, common_name) %>%
  group_by(deployment_id, detection_date, common_name) %>%
  summarise(sequence_count = n(), .groups = "drop")

# Expand detection history to include all combinations of deployments, dates, and common names
detection_history_expanded <- detection_history %>%
  distinct(subproject_name, deployment_id, date, day_number) %>%
  tidyr::crossing(sequences_filtered %>% distinct(common_name)) %>%
  left_join(detections, by = c("deployment_id", "date" = "detection_date", "common_name")) %>%
  mutate(sequence_count = replace_na(sequence_count, 0))

# Create summaries per subproject of the detection rate for each species and the number of working cameras for each day
detection_summary <- detection_history_expanded %>%
  group_by(subproject_name, day_number) %>%
  summarise(
    number_of_cameras = n_distinct(deployment_id),
    .groups = "drop"
  ) %>%
  left_join(
    detection_history_expanded %>%
      group_by(subproject_name, day_number, common_name) %>%
      summarise(total_detections = sum(sequence_count), .groups = "drop") %>%
      pivot_wider(names_from = common_name, values_from = total_detections, values_fill = 0),
    by = c("subproject_name", "day_number")
  ) %>%
  mutate(across(-c(subproject_name, day_number, number_of_cameras), ~ . / number_of_cameras, .names = "{.col}"))

# Prompt the user to choose the time range for graphs
time_range <- readline(prompt = "Please enter the time range for graphs: 1 week, 1 month, or all data: ")

# Set the maximum day number for graph display based on user input
if (tolower(time_range) == "1 week") {
  max_day_number <- 7
} else if (tolower(time_range) == "1 month") {
  max_day_number <- 30
} else {
  max_day_number <- max(detection_summary$day_number, na.rm = TRUE)
}

# Loop through each species with at least 10 detections and create plots
detection_summary_species <- detection_history_expanded %>%
  group_by(common_name) %>%
  summarise(total_detections = sum(sequence_count, na.rm = TRUE)) %>%
  filter(total_detections >= 10)

for (species in detection_summary_species$common_name) {
  detection_summary_species_data <- detection_summary %>%
    select(subproject_name, day_number, number_of_cameras, all_of(species)) %>%
    filter(!is.na(.data[[species]]) & subproject_name != "CEBO")
  
  if (nrow(detection_summary_species_data) > 0) {
    avg_number_of_cameras <- detection_summary_species_data %>% group_by(day_number) %>% summarise(avg_cameras = mean(number_of_cameras, na.rm = TRUE))
    avg_species_detection <- detection_summary_species_data %>% group_by(day_number) %>% filter(n() > 1) %>% summarise(
      avg_detection = mean(.data[[species]], na.rm = TRUE),
      lower_ci = avg_detection - qt(0.975, df = n() - 1) * sd(.data[[species]], na.rm = TRUE) / sqrt(n()),
      upper_ci = avg_detection + qt(0.975, df = n() - 1) * sd(.data[[species]], na.rm = TRUE) / sqrt(n())
    )
    avg_species_rate <- mean(detection_summary_species_data[[species]], na.rm = TRUE)
    
    p <- ggplot(detection_summary_species_data %>% filter(day_number <= max_day_number), aes(x = day_number)) +
      geom_line(data = avg_species_detection %>% filter(day_number <= max_day_number), aes(x = day_number, y = avg_detection), color = "red", linetype = "dotted") +
      geom_ribbon(data = avg_species_detection %>% filter(day_number <= max_day_number), aes(x = day_number, ymin = ifelse(lower_ci < 0, 0, lower_ci), ymax = upper_ci), fill = "red", alpha = 0.2) +
      geom_hline(yintercept = avg_species_rate, linetype = "dashed", color = "blue", size = 1) +
      scale_y_continuous(
        name = "Detection Rate",
        limits = c(0, max(avg_species_detection %>% filter(day_number <= max_day_number) %>% select(upper_ci, avg_detection) %>% unlist(), na.rm = TRUE) * 1.2)
      ) +
      labs(title = paste("Detection Rates for", species, "Over Time"),
           x = "Day Number") +
      theme_minimal(base_rect_size = 0) +
      theme(
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_blank()
      ) +
      coord_cartesian(xlim = c(0, max_day_number))
    print(p)
    ggsave(paste0("detection_rates_", gsub(" ", "_", species), "_plot.png"), plot = p)
  }
}
