##############################
#
# eBird Checklist Prep
# Ian Becker
# 1/1/2026
# 
##############################

# This script is used to prep and extract eBird checklist data for use
# counting checklists per college campus

library(auk)
library(dplyr)
library(sf)
library(lubridate)

##############################
# FILE PREP
##############################

# eBird data files 

ebd_dir <- "PATH_TO_EBIRD_DATA"  # Directory containing state EBD files
sampling_dir <- "PATH_TO_SAMPLING_FILES"  # Directory containing sampling event files

# States to process

states <- c("Texas", "Oklahoma", "Kansas")
state_abbr <- c("TX", "OK", "KS")

# Output directory

output_dir <- "PATH_TO_OUTPUT"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

##############################
# FILTERING PARAMETERS
##############################

# Date range: 2015-01-01 to 2025-12-31

date_range <- c(as.Date("2015-01-01"), as.Date("2025-12-31"))

# Travel distance threshold (meters)

max_travel_distance <- 20000  # 20 km in meters

##############################
# PROCESS EACH STATE
##############################

process_state <- function(state_name, state_code) {
  
  cat("\n===========================================\n")
  cat("Processing:", state_name, "\n")
  cat("===========================================\n")
  
  # Construct file paths
  
  ebd_file <- file.path(ebd_dir, paste0("ebd_", state_code, "_relXXX_YYYYMMDD.txt"))
  sampling_file <- file.path(sampling_dir, paste0("ebd_sampling_relXXX_", state_code, "_YYYYMMDD.txt"))
  
  # Check if files exist
  
  if (!file.exists(ebd_file)) {
    cat("WARNING: EBD file not found:", ebd_file, "\n")
    return(NULL)
  }
  if (!file.exists(sampling_file)) {
    cat("WARNING: Sampling file not found:", sampling_file, "\n")
    return(NULL)
  }
  
  cat("Input EBD file:", ebd_file, "\n")
  cat("Input sampling file:", sampling_file, "\n\n")
  
  # Read the data directly

  cat("Reading eBird data (this may take several minutes)...\n")
  
  ebd_data <- auk_ebd(ebd_file, file_sampling = sampling_file) %>%
    read_ebd()
  
  cat("  Initial observations:", nrow(ebd_data), "\n")
  
  # Apply all filters in memory
  
  cat("Applying filters...\n")
  
  ebd_cleaned <- ebd_data %>%
    
    # Filter 1: Complete checklists only
    
    filter(all_species_reported == TRUE) %>%
    {cat("  After complete checklist filter:", nrow(.), "\n"); .} %>%
    
    # Filter 2: Date range
    
    filter(observation_date >= date_range[1] & observation_date <= date_range[2]) %>%
    {cat("  After date filter:", nrow(.), "\n"); .} %>%
    
    # Filter 3: Get unique checklists (one row per sampling event)
    
    distinct(checklist_id, .keep_all = TRUE) %>%
    {cat("  After unique checklists:", nrow(.), "\n"); .} %>%
    
    # Filter 4: Travel distance <= 20km
    
    filter(is.na(effort_distance_km) | effort_distance_km <= 20) %>%
    {cat("  After travel distance filter:", nrow(.), "\n"); .}
  
  # Cleanup
  
  rm(ebd_data)
  gc(verbose = FALSE)
  
  cat("Processing complete for", state_name, "\n")
  
  return(ebd_cleaned)
}

# Process all states

cat("\n===========================================\n")
cat("PROCESSING ALL STATES\n")
cat("===========================================\n")

ebird_data <- list()

for (i in seq_along(states)) {
  ebird_data[[state_abbr[i]]] <- process_state(states[i], state_abbr[i])
  
  # Cleanup after each state
  
  cat("\nMemory cleanup...\n")
  gc(verbose = FALSE)
  
  cat("Completed", i, "of", length(states), "states\n")
}

##############################
# COMBINE ALL STATES
##############################

cat("\n===========================================\n")
cat("Combining all states...\n")
cat("===========================================\n")

# Combine all states

ebird_combined <- bind_rows(ebird_data)

# Cleanup: Remove the list and individual state data

rm(ebird_data)
gc(verbose = FALSE)

cat("\n===========================================\n")
cat("SUMMARY\n")
cat("===========================================\n")
cat("Total checklists across all states:", nrow(ebird_combined), "\n")
cat("Date range:", min(ebird_combined$observation_date), "to", 
    max(ebird_combined$observation_date), "\n")
cat("Unique observers:", length(unique(ebird_combined$observer_id)), "\n")

##############################
# SAVE DATASET
##############################

cat("\n===========================================\n")
cat("Saving outputs...\n")
cat("===========================================\n")

# Save as RDS for easy loading later

output_rds <- file.path(output_dir, "ebird_combined_2015_2025.rds")
saveRDS(ebird_combined, output_rds)
cat("\nSaved combined dataset to:", output_rds, "\n")

# Also save as CSV (may be large)

output_csv <- file.path(output_dir, "ebird_combined_2015_2025.csv")
write.csv(ebird_combined, output_csv, row.names = FALSE)
cat("Saved as CSV to:", output_csv, "\n")

# Final memory cleanup

rm(ebird_combined)
gc()

cat("\n===========================================\n")
cat("eBird data extraction complete!\n")
cat("===========================================\n")
cat("\nMemory cleaned up. Script finished successfully.\n")