##############################
#
# eBird Extraction + Campus Spatial Join
# Combined workflow - processes state by state
# College Campus Community Science Study
#
# MEMORY-EFFICIENT APPROACH:
# - Process one state at a time
# - Extract → Filter → Spatial Join → Count → Delete
# - Never keeps full dataset in memory
# - Only saves final campus counts (205 rows)
#
##############################

# Libraries
library(auk)
library(dplyr)
library(sf)
library(lubridate)
library(tidyr)

##############################
# STEP 1: Setup paths
##############################

# eBird data directory
ebd_dir <- "/home/ianbecker01/campus_cosci/data"
sampling_dir <- "/home/ianbecker01/campus_cosci/data"

# Campus polygon shapefile
campus_shapefile <- "/home/ianbecker01/campus_cosci/data/campus_polygons_complete"  # UPDATE THIS PATH

# Output directory
output_dir <- "/home/ianbecker01/campus_cosci/data"

# States to process
states <- c("Texas", "Oklahoma", "Kansas")
state_abbr <- c("TX", "OK", "KS")

# Date range
date_range <- c(as.Date("2015-01-01"), as.Date("2025-12-31"))

##############################
# STEP 2: Load campus polygons (once)
##############################

cat("Loading campus polygon data...\n")
campus_polygons <- st_read(campus_shapefile, quiet = TRUE)

cat("Campus polygons loaded:", nrow(campus_polygons), "\n")
cat("Expected column: unitid\n")
cat("Columns in shapefile:", paste(names(campus_polygons), collapse = ", "), "\n\n")

# Ensure CRS is WGS84
if (st_crs(campus_polygons) != st_crs(4326)) {
  cat("Reprojecting campus polygons to WGS84...\n")
  campus_polygons <- st_transform(campus_polygons, crs = 4326)
}

##############################
# STEP 3: Process each state
##############################

process_state_and_join <- function(state_name, state_code) {
  
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
  
  # Apply additional filters
  ebd_filtered <- ebd_data %>%
    distinct(checklist_id, .keep_all = TRUE) %>%
    filter(is.na(effort_distance_km) | effort_distance_km <= 20)
  
  cat("  After filters (unique + ≤20km):", nrow(ebd_filtered), "\n")
  
  # Clean up full observation data
  rm(ebd_data)
  gc(verbose = FALSE)
  
  # === PART B: Spatial join to campuses ===
  
  cat("\nPart B: Spatial join to campus polygons\n")
  
  # Convert checklists to spatial points
  cat("  Converting checklists to spatial points...\n")
  ebird_sf <- st_as_sf(
    ebd_filtered,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )
  
  cat("  Spatial points created:", nrow(ebird_sf), "\n")
  
  # Spatial join (keep only checklists within campus polygons)
  cat("  Performing spatial join (this may take a few minutes)...\n")
  checklists_on_campus <- st_join(
    ebird_sf,
    campus_polygons %>% select(unitid),  # Only keep unitid from campus data
    join = st_within,
    left = FALSE  # Inner join - only keep matches
  )
  
  cat("  Checklists within campuses:", nrow(checklists_on_campus), "\n")
  
  # Clean up full spatial data
  rm(ebird_sf, ebd_filtered)
  gc(verbose = FALSE)
  
  # === PART C: Count checklists per campus ===
  
  cat("\nPart C: Counting checklists per campus\n")
  
  if (nrow(checklists_on_campus) > 0) {
    
    campus_counts <- checklists_on_campus %>%
      st_drop_geometry() %>%
      group_by(unitid) %>%
      summarise(
        checklist_count = n_distinct(checklist_id),
        state = state_name,
        .groups = "drop"
      )
    
    cat("  Campuses with checklists in", state_name, ":", nrow(campus_counts), "\n")
    cat("  Total checklists:", sum(campus_counts$checklist_count), "\n")
    
  } else {
    cat("  No checklists found on any campus in", state_name, "\n")
    campus_counts <- data.frame(
      unitid = character(),
      checklist_count = integer(),
      state = character()
    )
  }
  
  # Clean up spatial join data
  rm(checklists_on_campus)
  gc(verbose = FALSE)
  
  cat("\nState processing complete:", state_name, "\n")
  
  return(campus_counts)
}

##############################
# STEP 4: Process all states
##############################

cat("\n===========================================\n")
cat("PROCESSING ALL STATES\n")
cat("===========================================\n")

state_counts_list <- list()

for (i in seq_along(states)) {
  state_counts_list[[i]] <- process_state_and_join(states[i], state_abbr[i])
  
  cat("\nMemory cleanup after", states[i], "...\n")
  gc(verbose = FALSE)
  
  cat("Completed", i, "of", length(states), "states\n")
}

##############################
# STEP 5: Combine all state counts
##############################

cat("\n===========================================\n")
cat("COMBINING STATE COUNTS\n")
cat("===========================================\n")

# Remove NULL entries (states that were skipped)
state_counts_list <- state_counts_list[!sapply(state_counts_list, is.null)]

if (length(state_counts_list) > 0) {
  
  # Combine all state counts
  all_counts <- bind_rows(state_counts_list)
  
  # Sum counts across states for each campus
  final_counts <- all_counts %>%
    group_by(unitid) %>%
    summarise(
      checklist_count = sum(checklist_count),
      states_with_checklists = paste(unique(state), collapse = ", "),
      .groups = "drop"
    )
  
  # Join back to full campus list (including those with 0 checklists)
  campus_attributes <- campus_polygons %>%
    st_drop_geometry() %>%
    select(unitid, everything())
  
  final_dataset <- campus_attributes %>%
    left_join(final_counts, by = "unitid") %>%
    mutate(
      checklist_count = replace_na(checklist_count, 0),
      states_with_checklists = replace_na(states_with_checklists, "none")
    )
  
  ##############################
  # STEP 6: Save results
  ##############################
  
  cat("\n===========================================\n")
  cat("FINAL RESULTS\n")
  cat("===========================================\n")
  
  cat("Total campuses:", nrow(final_dataset), "\n")
  cat("Campuses with >0 checklists:", sum(final_dataset$checklist_count > 0), "\n")
  cat("Campuses with 0 checklists:", sum(final_dataset$checklist_count == 0), "\n")
  cat("Total checklists:", sum(final_dataset$checklist_count), "\n\n")
  
  # Summary by state
  cat("Campuses with checklists by state:\n")
  state_summary <- final_dataset %>%
    filter(checklist_count > 0) %>%
    count(states_with_checklists)
  print(state_summary)
  
  # Save final counts
  output_file <- file.path(output_dir, "campus_checklist_counts.csv")
  write.csv(final_dataset, output_file, row.names = FALSE)
  cat("\nSaved to:", output_file, "\n")
  
  # Save filtered version (only campuses with >0 checklists)
  final_filtered <- final_dataset %>%
    filter(checklist_count > 0)
  
  output_filtered <- file.path(output_dir, "campus_checklist_counts_filtered.csv")
  write.csv(final_filtered, output_filtered, row.names = FALSE)
  cat("Saved filtered to:", output_filtered, "\n")
  
  # Top 10 campuses
  cat("\nTop 10 campuses by checklist count:\n")
  top10 <- final_dataset %>%
    arrange(desc(checklist_count)) %>%
    head(10) %>%
    select(unitid, inst_name, checklist_count, states_with_checklists)
  print(top10)
  
} else {
  cat("\nNo data processed - check if eBird files exist!\n")
}

cat("\n===========================================\n")
cat("SCRIPT COMPLETE!\n")
cat("===========================================\n")
