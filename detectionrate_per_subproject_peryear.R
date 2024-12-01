##This calculates the camera days, detections, and detection rate per species-subproject-year.
library(dplyr)
library(lubridate)

# Load the data
deployments <- read.csv('deployments.csv')
sequences <- read.csv('sequences.csv')

# Convert dates in deployments to Date type and calculate duration
filtered_deployments <- deployments %>%
  mutate(
    start_date = as.POSIXct(start_date, format = "%Y-%m-%d %H:%M:%S"),
    end_date = as.POSIXct(end_date, format = "%Y-%m-%d %H:%M:%S"),
    year = year(start_date),
    duration = as.numeric(difftime(end_date, start_date, units = "days"))
  )

# Resolve many-to-many relationship by using distinct deployments
filtered_deployments_unique <- filtered_deployments %>%
  distinct(deployment_id, .keep_all = TRUE)

# Merge sequences with deployments and convert date-time fields
sequences_with_subproject <- sequences %>%
  left_join(filtered_deployments_unique, by = "deployment_id") %>%
  mutate(
    start_time = as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S"),
    end_time = as.POSIXct(end_time, format = "%Y-%m-%d %H:%M:%S"),
    date = as.Date(start_time),
    year = year(date)
  )

# Create a grid of all possible subproject, species, and year combinations (only for years with data)
available_years <- unique(filtered_deployments$year)
subproject_species_year_grid <- expand.grid(
  subproject_name = unique(filtered_deployments$subproject_name),
  common_name = unique(sequences$common_name),
  year = available_years,
  stringsAsFactors = FALSE
)

# Sum detections for each species in each subproject-year
detections_per_species_subproject_year <- sequences_with_subproject %>%
  group_by(subproject_name, common_name, year) %>%
  summarise(detections = n(), .groups = 'drop')

# Merge the grid with detections to include zeros (only for years with data)
detections_complete <- subproject_species_year_grid %>%
  left_join(detections_per_species_subproject_year, by = c("subproject_name", "common_name", "year")) %>%
  mutate(detections = ifelse(is.na(detections), 0, detections))

# Calculate total camera time for each subproject-year
total_time_per_subproject_year <- filtered_deployments %>%
  group_by(subproject_name, year) %>%
  summarise(total_camera_time = sum(duration), .groups = 'drop')

# Filter out subproject-years without any camera time
detections_filtered <- detections_complete %>%
  semi_join(total_time_per_subproject_year, by = c("subproject_name", "year"))

# Merge detections and total camera time, and calculate detection rate
detection_rate <- detections_filtered %>%
  left_join(total_time_per_subproject_year, by = c("subproject_name", "year")) %>%
  mutate(detection_rate = detections / total_camera_time)

# Aggregate distinct species information
species_info <- sequences %>%
  select(common_name, class, order, family) %>%
  distinct()

# Expand detection rate data with species information
detection_rate_expanded <- detection_rate %>%
  left_join(species_info, by = "common_name")

# Calculate average latitude and longitude for each subproject
avg_lat_long <- filtered_deployments %>%
  group_by(subproject_name) %>%
  summarise(
    avg_latitude = mean(latitude, na.rm = TRUE),
    avg_longitude = mean(longitude, na.rm = TRUE),
    .groups = 'drop'
  )

# Merge with detection rate data
detection_rate_final <- detection_rate_expanded %>%
  left_join(avg_lat_long, by = "subproject_name")

# Export the final result
write.csv(detection_rate_final, "detection_rate_subproject_year.csv", row.names = FALSE)
