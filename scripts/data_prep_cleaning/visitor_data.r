##############################
#
# Unique Visitor Comparison: Campus vs Hotspot
# Counts unique observer_ids at campuses vs non-campus hotspots
# Output: CSV for t-test comparison
#
# College Campus Community Science Study
# Revised analysis addressing reviewer concerns
#
##############################

# Libraries
library(auk)
library(dplyr)
library(sf)
library(tidyr)

##############################
# STEP 1: Setup paths
##############################

# eBird data directory - UPDATE THESE PATHS
ebd_dir <- "/home/ianbecker01/campus_cosci/data"
sampling_dir <- "/home/ianbecker01/campus_cosci/data"

# Campus polygon shapefile - UPDATE THIS PATH
campus_shapefile <- "/home/ianbecker01/campus_cosci/data/campus_polygons.shp"

# Output directory
output_dir <- "/home/ianbecker01/campus_cosci/output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# States to process (three-state focused approach)
states <- c("Texas", "Oklahoma", "Kansas")
state_abbr <- c("TX", "OK", "KS")

# Date range (2015-2024 per your revised methodology)
date_range <- c(as.Date("2015-01-01"), as.Date("2024-12-31"))

##############################
# STEP 2: Load campus polygons (once)
##############################

cat("Loading campus polygon data...\n")
campus_polygons <- st_read(campus_shapefile, quiet = TRUE)

cat("Campus polygons loaded:", nrow(campus_polygons), "\n")
cat("Columns in shapefile:", paste(names(campus_polygons), collapse = ", "), "\n\n")

# Ensure CRS is WGS84
if (st_crs(campus_polygons) != st_crs(4326)) {
  cat("Reprojecting campus polygons to WGS84...\n")
  campus_polygons <- st_transform(campus_polygons, crs = 4326)
}

##############################
# STEP 3: Process each state - count unique visitors
##############################

process_state_visitors <- function(state_name, state_code) {
  
  cat("\n===========================================\n")
  cat("Processing:", state_name, "\n")
  cat("===========================================\n")
  
  # Construct file paths
  ebd_file <- file.path(ebd_dir, paste0("ebd_US-", state_code, "_smp_relNov-2025.txt"))
  sampling_file <- file.path(sampling_dir, paste0("ebd_US-", state_code, "_smp_relNov-2025_sampling.txt"))
  
  # Check if files exist
  if (!file.exists(ebd_file)) {
    cat("WARNING: EBD file not found - skipping", state_name, "\n")
    return(NULL)
  }
  if (!file.exists(sampling_file)) {
    cat("WARNING: Sampling file not found - skipping", state_name, "\n")
    return(NULL)
  }
  
  # === PART A: Extract and filter eBird data ===
  
  cat("\nPart A: Extracting eBird data\n")
  
  temp_output <- file.path(output_dir, paste0("temp_", state_code, ".txt"))
  temp_sampling <- file.path(output_dir, paste0("temp_", state_code, "_sampling.txt"))
  
  cat("  Filtering for complete checklists and date range...\n")
  auk_ebd(ebd_file, file_sampling = sampling_file) %>%
    auk_complete() %>%
    auk_date(date = date_range) %>%
    auk_filter(file = temp_output, 
               file_sampling = temp_sampling,
               overwrite = TRUE)
  
  cat("  Reading filtered data...\n")
  ebd_data <- read_ebd(temp_output)
  
  # Delete temp files
  file.remove(temp_output)
  file.remove(temp_sampling)
  
  cat("  Initial observations:", nrow(ebd_data), "\n")
  
  # Apply additional filters (unique checklists, ≤20km travel)
  ebd_filtered <- ebd_data %>%
    distinct(checklist_id, .keep_all = TRUE) %>%
    filter(is.na(effort_distance_km) | effort_distance_km <= 20)
  
  cat("  After filters (unique + ≤20km):", nrow(ebd_filtered), "\n")
  
  # Clean up
  rm(ebd_data)
  gc(verbose = FALSE)
  
  # === PART B: Campus unique visitors ===
  
  cat("\nPart B: Counting unique visitors at campuses\n")
  
  # Convert checklists to spatial points
  ebird_sf <- st_as_sf(
    ebd_filtered,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )
  
  # Spatial join to campus polygons
  cat("  Performing spatial join to campuses...\n")
  checklists_on_campus <- st_join(
    ebird_sf,
    campus_polygons %>% select(unitid, inst_name),
    join = st_within,
    left = FALSE
  )
  
  cat("  Checklists within campuses:", nrow(checklists_on_campus), "\n")
  
  # Count unique visitors per campus
  if (nrow(checklists_on_campus) > 0) {
    campus_visitors <- checklists_on_campus %>%
      st_drop_geometry() %>%
      group_by(unitid, inst_name) %>%
      summarise(
        unique_visitors = n_distinct(observer_id),
        checklist_count = n_distinct(checklist_id),
        state = state_name,
        location_type = "campus",
        .groups = "drop"
      )
    cat("  Campuses with visitors:", nrow(campus_visitors), "\n")
    cat("  Total unique campus visitors:", sum(campus_visitors$unique_visitors), "\n")
  } else {
    campus_visitors <- data.frame(
      unitid = character(),
      inst_name = character(),
      unique_visitors = integer(),
      checklist_count = integer(),
      state = character(),
      location_type = character()
    )
  }
  
  # Cleanup campus spatial objects
  rm(ebird_sf, checklists_on_campus)
  gc(verbose = FALSE)
  
  # === PART C: Non-campus hotspot unique visitors ===
  
  cat("\nPart C: Counting unique visitors at non-campus hotspots\n")
  
  # Filter to hotspots only (locality_type == "H")
  hotspot_checklists <- ebd_filtered %>%
    filter(locality_type == "H")
  
  # Done with full filtered data
  rm(ebd_filtered)
  gc(verbose = FALSE)
  
  cat("  Hotspot checklists:", nrow(hotspot_checklists), "\n")
  
  # Convert to spatial
  hotspot_sf <- st_as_sf(
    hotspot_checklists,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )
  
  # Done with hotspot_checklists
  rm(hotspot_checklists)
  gc(verbose = FALSE)
  
  # Remove any hotspots that fall within campus polygons
  cat("  Removing hotspots within campus boundaries...\n")
  hotspot_in_campus <- st_join(
    hotspot_sf,
    campus_polygons %>% select(unitid),
    join = st_within,
    left = TRUE
  )
  
  # Keep only those NOT in a campus
  non_campus_hotspots <- hotspot_in_campus %>%
    filter(is.na(unitid)) %>%
    st_drop_geometry()
  
  cat("  Non-campus hotspot checklists:", nrow(non_campus_hotspots), "\n")
  
  # Count unique visitors per hotspot
  if (nrow(non_campus_hotspots) > 0) {
    hotspot_visitors <- non_campus_hotspots %>%
      group_by(locality_id, locality) %>%
      summarise(
        unique_visitors = n_distinct(observer_id),
        checklist_count = n_distinct(checklist_id),
        state = state_name,
        location_type = "hotspot",
        .groups = "drop"
      ) %>%
      rename(location_id = locality_id, location_name = locality)
    
    cat("  Unique hotspots:", nrow(hotspot_visitors), "\n")
    cat("  Total unique hotspot visitors:", sum(hotspot_visitors$unique_visitors), "\n")
  } else {
    hotspot_visitors <- data.frame(
      location_id = character(),
      location_name = character(),
      unique_visitors = integer(),
      checklist_count = integer(),
      state = character(),
      location_type = character()
    )
  }
  
  # === PART D: Standardize and combine ===
  
  # Rename campus columns to match hotspot structure
  campus_visitors_std <- campus_visitors %>%
    rename(location_id = unitid, location_name = inst_name)
  
  # Combine
  combined <- bind_rows(campus_visitors_std, hotspot_visitors)
  
  # Final cleanup
  rm(hotspot_sf, hotspot_in_campus, non_campus_hotspots)
  gc(verbose = FALSE)
  
  cat("\nState processing complete:", state_name, "\n")
  
  return(combined)
}

##############################
# STEP 4: Process all states
##############################

cat("\n===========================================\n")
cat("PROCESSING ALL STATES\n")
cat("===========================================\n")

all_results <- list()

for (i in seq_along(states)) {
  all_results[[i]] <- process_state_visitors(states[i], state_abbr[i])
  
  cat("\nMemory cleanup after", states[i], "...\n")
  gc(verbose = FALSE)
  
  cat("Completed", i, "of", length(states), "states\n")
}

##############################
# STEP 5: Combine and save results
##############################

cat("\n===========================================\n")
cat("COMBINING RESULTS\n")
cat("===========================================\n")

# Remove NULL entries
all_results <- all_results[!sapply(all_results, is.null)]

if (length(all_results) > 0) {
  
  final_data <- bind_rows(all_results)
  
  # Sample hotspots to match campus count
  set.seed(123)  # For reproducibility
  
  n_campuses <- sum(final_data$location_type == "campus")
  cat("Number of campuses:", n_campuses, "\n")
  
  campus_data <- final_data %>% filter(location_type == "campus")
  hotspot_data <- final_data %>% 
    filter(location_type == "hotspot") %>%
    slice_sample(n = n_campuses)
  
  cat("Sampled hotspots to match:", nrow(hotspot_data), "\n")
  
  # Recombine with matched sample
  final_data <- bind_rows(campus_data, hotspot_data)
  
  # Summary statistics
  cat("\n--- SUMMARY ---\n")
  cat("Total locations:", nrow(final_data), "\n")
  
  summary_by_type <- final_data %>%
    group_by(location_type) %>%
    summarise(
      n_locations = n(),
      mean_visitors = mean(unique_visitors),
      median_visitors = median(unique_visitors),
      sd_visitors = sd(unique_visitors),
      total_visitors = sum(unique_visitors),
      .groups = "drop"
    )
  
  print(summary_by_type)
  
  # Save full dataset
  output_file <- file.path(output_dir, "unique_visitors_comparison.csv")
  write.csv(final_data, output_file, row.names = FALSE)
  cat("\nFull data saved to:", output_file, "\n")
  
  # Save summary for quick reference
  summary_file <- file.path(output_dir, "visitor_summary_by_type.csv")
  write.csv(summary_by_type, summary_file, row.names = FALSE)
  cat("Summary saved to:", summary_file, "\n")
  
  # Quick t-test preview
  cat("\n--- T-TEST PREVIEW ---\n")
  campus_data <- final_data %>% filter(location_type == "campus")
  hotspot_data <- final_data %>% filter(location_type == "hotspot")
  
  if (nrow(campus_data) > 1 & nrow(hotspot_data) > 1) {
    t_result <- t.test(campus_data$unique_visitors, hotspot_data$unique_visitors)
    cat("Campus mean:", round(mean(campus_data$unique_visitors), 2), "\n")
    cat("Hotspot mean:", round(mean(hotspot_data$unique_visitors), 2), "\n")
    cat("t =", round(t_result$statistic, 3), "\n")
    cat("p-value =", format(t_result$p.value, scientific = TRUE, digits = 3), "\n")
    cat("95% CI:", round(t_result$conf.int[1], 2), "to", round(t_result$conf.int[2], 2), "\n")
  }
  
} else {
  cat("\nNo data processed - check if eBird files exist!\n")
}

cat("\n===========================================\n")
cat("SCRIPT COMPLETE!\n")
cat("===========================================\n")