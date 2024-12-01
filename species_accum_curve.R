#Plots species accumulation curves for each subproject.
#Defaut settings plot only mammals and exclude domestics
#also returns a summary of what species are detected in each subproject, useful QC step.

# Load required libraries
library(iNEXT)
library(ggplot2)  
library(dplyr)  

# Load the deployment and sequence tables
deployments <- read.csv("deployments.csv")
sequences <- read.csv("sequences.csv")

# Filter sequences for Class Mammalia and for records identified to the species level
filtered_sequences <- subset(sequences, class == "Mammalia" & !is.na(genus) & !is.na(species) & genus != "" & species != "")

# Remove records with empty genus or species names
filtered_sequences <- filtered_sequences[filtered_sequences$genus != "" & filtered_sequences$species != "", ]

# Merge sequences with deployments to include subproject information
merged_data <- merge(filtered_sequences, deployments[, c("deployment_id", "subproject_name")], by = "deployment_id")

# Remove specific species
species_to_remove <- c("Homo sapiens", "Sus scrofa scrofa", "Bos taurus", "Felis catus", "Canis familiaris")
merged_data <- merged_data[!paste(merged_data$genus, merged_data$species) %in% species_to_remove, ]

# Prepare species incidence data for all subprojects
species_summary <- list()
data_for_iNEXT <- list()
subproject_names <- unique(merged_data$subproject_name)

# If subproject names are missing, treat it as one subproject
if (all(is.na(subproject_names)) || (length(subproject_names) == 1 && subproject_names == "")) {
  subproject_names <- "All"
  merged_data$subproject_name <- "All"
}

for (subproject_name in subproject_names) {
  subproject_data <- merged_data[merged_data$subproject_name == subproject_name, ]
  if (nrow(subproject_data) > 0) {
    # Create a presence-absence matrix for species by deployment
    species_incidence <- table(subproject_data$deployment_id, paste(subproject_data$genus, subproject_data$species))
    if (nrow(species_incidence) > 0 && ncol(species_incidence) >= 5) {  # Filter out subprojects with fewer than 5 species
      # Sum the presence across deployments to get the number of deployments each species was found in
      species_incidence_list <- colSums(species_incidence > 0)
      # Ensure the first element is the number of sampling units (deployments)
      data_for_iNEXT[[subproject_name]] <- c(nrow(species_incidence), as.numeric(species_incidence_list))
      
      # Create a summary table of species and their number of detections for each subproject
      species_summary[[subproject_name]] <- subproject_data %>%
        dplyr::group_by(genus, species) %>%  # Grouping by both 'genus' and 'species'
        dplyr::summarise(detections = n(), .groups = 'drop') %>%
        dplyr::arrange(desc(detections))
    }
  }
}

# Display species summary tables for verification
if (length(species_summary) > 0) {
  for (subproject_name in subproject_names) {
    if (!is.null(species_summary[[subproject_name]])) {
      cat("\nSpecies summary for subproject:", subproject_name, "\n")
      print(species_summary[[subproject_name]], n = Inf)  # Print the entire table
    }
  }
} else {
  cat("No valid species data available for any subproject.\n")
}

# Run iNEXT for incidence data (set datatype to "incidence_freq")
if (length(data_for_iNEXT) > 0) {
  # Validate data structure for iNEXT
  valid_data_for_iNEXT <- data_for_iNEXT[sapply(data_for_iNEXT, function(x) length(x) > 1)]
  if (length(valid_data_for_iNEXT) > 0) {
    # Run the iNEXT analysis
    result <- iNEXT(valid_data_for_iNEXT, q = 0, datatype = "incidence_freq")
  } else {
    cat("No valid data available for iNEXT analysis.\n")
  }
} else {
  cat("No valid data available for iNEXT analysis.\n")
}

# Display the head of the iNEXT result data for verification
if (exists("result") && !is.null(result)) {
  result_df_list <- lapply(result$iNextEst, function(x) {
    x_df <- as.data.frame(x)
    return(x_df)
  })
  
  # Make sure all data frames have the same structure by binding with dplyr::bind_rows
  result_df <- bind_rows(result_df_list, .id = "Subproject")
  cat("\nHead of the iNEXT result data:\n")
  print(head(result_df))
}

# Export the iNEXT result data to CSV
if (exists("result_df") && !is.null(result_df)) {
  write.csv(result_df, "species_accumulation_data.csv", row.names = FALSE)
}

# Plot the species accumulation curves with confidence intervals and custom labels
if (exists("result") && !is.null(result)) {
  plot_result <- ggiNEXT(result, type = 1, se = TRUE) +
    labs(color = "Subproject") +
    theme_minimal() +  # Use a minimal theme for better visualization
    theme(legend.position = "bottom", legend.title = element_text(face = "bold")) +  # Position legend at the bottom
    scale_color_manual(values = scales::hue_pal()(length(names(valid_data_for_iNEXT))),
                       labels = names(valid_data_for_iNEXT)) +
    geom_line(size = 0.5) +  # Make accumulation lines thinner
    geom_line(aes(linetype = "Extrapolated"), size = 0.5, linetype = "dashed") +  # Use dashed lines for extrapolation
    scale_linetype_manual(values = c("solid", "dashed"))  # Use solid lines for observed and dashed for extrapolated

  # Print the plot
  print(plot_result)
} else {
  cat("No valid result available for plotting.\n")
}
