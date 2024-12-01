#this plots activity graphs and calculates the % of the day active for each species.

# Load required libraries
library(dplyr)
library(tidyr)
library(activity)
library(gridExtra)  # For combining multiple plots

# Load the dataset
camera_data <- read.csv("sequences.csv")

# Create a table of detections per species
species_detections <- camera_data %>% 
  mutate(species = if_else(is.na(species), "Unknown", species),
         common_name = if_else(is.na(common_name), "Unknown", common_name)) %>%
  count(family, genus, species, common_name) %>%
  arrange(desc(n)) %>%
  rename(detections_count = n)

# Initialize an empty dataframe to store activity percentage results
activity_summary <- data.frame(
  family = character(),
  genus = character(),
  species = character(),
  common_name = character(),
  percent_active = numeric(),
  stringsAsFactors = FALSE
)

# Initialize an empty list to store activity plots
activity_plots <- list()

# Loop through each species to create activity plots and calculate % time active
for (i in 1:nrow(species_detections)) {
  # Filter data for the current species
  species_data <- camera_data %>%
    filter(family == species_detections$family[i],
           genus == species_detections$genus[i],
           species == species_detections$species[i])
  
  # Check if there are enough detections
  if (nrow(species_data) >= 25) {
    # Check if 'start_time' column exists in the dataset
    if ("start_time" %in% colnames(species_data)) {
      # Convert 'start_time' to POSIXct
      species_data$start_time <- as.POSIXct(species_data$start_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
      
      # Extract the time in hours and convert to radians
      time_in_hours <- as.numeric(format(species_data$start_time, "%H")) + 
        as.numeric(format(species_data$start_time, "%M")) / 60
      time_in_radians <- (time_in_hours / 24) * 2 * pi
      
      # Remove NA values from time data
      time_in_radians <- na.omit(time_in_radians)
      
      # Check if there is enough data to fit the model
      if (length(time_in_radians) >= 25) {
        # Fit the activity model
        density_fit <- fitact(time_in_radians)
        
        # Extract the activity level (proportion of time active)
        activity_level <- density_fit@act
        
        # Convert to percentage
        percent_active <- activity_level * 100
        
        # Add the result to the activity_summary dataframe
        activity_summary <- rbind(activity_summary, data.frame(
          family = species_detections$family[i],
          genus = species_detections$genus[i],
          species = species_detections$species[i],
          common_name = species_detections$common_name[i],
          percent_active = percent_active,
          stringsAsFactors = FALSE
        ))
        
        # Create the activity pattern plot and record it
        plot_title <- paste("Activity Pattern for", species_detections$genus[i], species_detections$species[i], "(n =", nrow(species_data), ")")
        plot(density_fit, main = plot_title)  # Removed unsupported 'ci' parameter
        
        # Capture the plot
        activity_plots[[length(activity_plots) + 1]] <- recordPlot()
        
        # Display the plot in the viewer to confirm it was generated
        replayPlot(activity_plots[[length(activity_plots)]])
      } else {
        message(paste("Not enough data to fit the activity model for", species_detections$genus[i], species_detections$species[i]))
      }
    } else {
      message("The 'start_time' column was not found in the dataset. Please ensure the dataset includes a 'start_time' column with detection times in the format 'YYYY-MM-DD HH:MM:SS'.")
    }
  } else {
    message(paste("Skipping species", species_detections$genus[i], species_detections$species[i], "- only", nrow(species_data), "detections available (minimum required: 25)"))
  }
}

# Combine species detections summary and activity summary into one table
species_combined_summary <- species_detections %>%
  left_join(activity_summary, by = c("family", "genus", "species", "common_name"))

# Write the combined summary to a CSV
write.csv(species_combined_summary, "species_combined_summary.csv", row.names = FALSE)

# Create a table reporting the number of detections per common name per hour of the day
hourly_detections <- camera_data %>%
  mutate(species = if_else(is.na(species), "Unknown", species),
         common_name = if_else(is.na(common_name), "Unknown", common_name)) %>%
  mutate(hour = as.numeric(format(as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%H"))) %>%
  count(common_name, hour) %>%
  complete(common_name, hour = 0:23, fill = list(n = 0)) %>%
  arrange(common_name, hour)

# Write the hourly detection summary to a CSV
write.csv(hourly_detections, "hourly_detections_summary.csv", row.names = FALSE)

# Save all activity plots to a single PDF file in a grid format (3 columns per row)
if (length(activity_plots) > 0) {
  pdf("combined_activity_plots.pdf", width = 11, height = 8.5)
  for (plot in activity_plots) {
    replayPlot(plot)
  }
  dev.off()
} else {
  message("No activity plots were generated due to insufficient data for all species.")
}
