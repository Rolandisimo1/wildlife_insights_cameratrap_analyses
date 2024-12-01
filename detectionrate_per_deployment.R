library(dplyr)
library(ggplot2)
library(lubridate)

# Load the datasets
deployments <- read.csv("deployments.csv")  # Replace with the correct file path
sequences <- read.csv("sequences.csv")  # Replace with the correct file path

# Convert the start and end dates to Date format and calculate the duration in days
deployments$start_date <- as.Date(deployments$start_date, format = "%Y-%m-%d")
deployments$end_date <- as.Date(deployments$end_date, format = "%Y-%m-%d")
deployments$duration_days <- as.numeric(difftime(deployments$end_date, deployments$start_date, units = "days"))

# Count the number of detections for each species (common name) for each deployment ID
detections <- sequences %>%
  filter(is_blank == 0) %>%  # Filter out blanks
  group_by(deployment_id, common_name) %>%
  summarise(detection_count = n()) %>%
  ungroup()

# Calculate the total number of detections for each common name
total_detections <- detections %>%
  group_by(common_name) %>%
  summarise(total_detection_count = sum(detection_count)) %>%
  ungroup()

# Filter out species with fewer than 10 detections in total
filtered_species <- total_detections %>%
  filter(total_detection_count >= 10) %>%
  select(common_name)

# Filter detections to keep only species with at least 10 total detections
detections_filtered <- detections %>%
  semi_join(filtered_species, by = "common_name")

# Ensure that each deployment_id is unique in the deployments dataset and include latitude, longitude, start_date, and project name
deployments_unique <- deployments %>%
  select(deployment_id, duration_days, latitude, longitude, start_date, project_id, subproject_name) %>%
  distinct()  # Remove duplicates

# Create all possible combinations of deployment IDs and species
all_combinations <- expand.grid(deployment_id = unique(deployments_unique$deployment_id),
                                common_name = unique(filtered_species$common_name))

# Join with the filtered detections data, filling in zeros where there were no detections
detections_complete <- all_combinations %>%
  left_join(detections_filtered, by = c("deployment_id", "common_name")) %>%
  mutate(detection_count = ifelse(is.na(detection_count), 0, detection_count))

# Merge the complete detection counts with the deployment data
detection_rates <- detections_complete %>%
  left_join(deployments_unique, by = "deployment_id") %>%
  mutate(detection_rate = detection_count / duration_days)

# Aggregate species information in sequences
species_info <- sequences %>%
  select(common_name, class, order, family) %>%
  distinct()

# Join detection_rate with species_info on common_name
detection_rates_final <- left_join(detection_rates, species_info, by = "common_name")

# Check the result
head(detection_rates_final)

# Print and/or export the final result as needed
write.csv(detection_rates_final, "detection_rate_deployment.csv", row.names = FALSE)
