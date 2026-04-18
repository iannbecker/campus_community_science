##############################
#
# Movement network analysis
# Ian Becker
# March 2026
#
##############################

# This script was originally formatted for a cluster and
# was used to gather weekly movement windows for all observers
# within our study. For privacy purposes, all data used has been 
# anonymized and filtered to remove any personally identifiable information.

library(tidyverse)
library(lubridate)
library(hms)
library(sf)
library(auk)

# =============================================================================
# DATA SETUP AND PREP
# =============================================================================

# Setup paths 

ebd_dir <- "PATH HERE"
sampling_dir <- "PATH HERE"
campus_shapefile <- "PATH HERE"
output_dir <- "PATH HERE"

date_range <- c(as.Date("2015-01-01"), as.Date("2025-12-31"))

# Load campus and hotspot lists

locations <- read.csv(file.path(output_dir, "unique_visitors_comparison.csv"))

# Load campus polygons once

campus_polygons <- st_read(campus_shapefile, quiet = TRUE)

# Set seed for reproducibility

set.seed(123)

# =============================================================================
# FUNCTION TO GET TOP 10 OBSERVERS
# =============================================================================

get_top_observers <- function(checklists, n = 10) {
  observer_counts <- checklists %>%
    group_by(observer_id) %>%
    summarize(n_checklists = n_distinct(checklist_id), .groups = "drop") %>%
    arrange(desc(n_checklists))
  
  if (nrow(observer_counts) < n) {
    return(NULL)  
  }
  
  return(observer_counts %>% slice(1:n) %>% pull(observer_id))
}

# =============================================================================
# FUNCTION TO GET ALL AVAILABLE 7 DAY MOVEMENT WINDOWS
# =============================================================================

get_all_windows <- function(location_checklists, observer_ids) {
  windows <- location_checklists %>%
    filter(observer_id %in% observer_ids) %>%
    arrange(observer_id, observation_date, time) %>%
    group_by(observer_id) %>%
    mutate(
      is_new_window = if_else(row_number() == 1, TRUE, FALSE)   # First checklist always starts a window
    ) %>%
    ungroup()
  
  # Now find all window starts
  
  for (i in 2:nrow(windows)) {
    if (windows$is_new_window[i]) next  
    
    # Find the most recent window start for this observer
    
    same_observer <- windows$observer_id[i] == windows$observer_id[1:(i-1)]
    is_window_start <- windows$is_new_window[1:(i-1)]
    
    if (any(same_observer & is_window_start)) {
      last_window_idx <- max(which(same_observer & is_window_start))
      last_window_date <- windows$observation_date[last_window_idx]
      
      # Check if ≥7 days since that window start
      
      days_since <- as.numeric(windows$observation_date[i] - last_window_date)
      windows$is_new_window[i] <- days_since >= 7
    }
  }
  
  # Extract window starts
  
  window_starts <- windows %>%
    filter(is_new_window) %>%
    select(observer_id, start_date = observation_date, start_time = time,
           start_lat = latitude, start_lon = longitude) %>%
    mutate(window_id = row_number())
  
  return(window_starts)
}

# =============================================================================
# FUNCTION TO TRACK MOVEMENT FOR ALL AVAILABLE WINDOWS
# =============================================================================

track_all_movement <- function(all_windows, all_ebd, location_name, location_type) {
  
  if (nrow(all_windows) == 0) return(NULL)
  
  movement_list <- list()
  
  for (i in 1:nrow(all_windows)) {
    window <- all_windows[i, ]
    
    # Get ALL checklists within 7 days AFTER this window start
    
    subsequent <- all_ebd %>%
      filter(
        observer_id == window$observer_id,
        observation_date >= window$start_date,
        observation_date <= window$start_date + 7
      ) %>%
      
      # Exclude checklists on start date that are before/at start time
      
      filter(!(observation_date == window$start_date & time <= window$start_time)) %>%
      arrange(observation_date, time) %>%
      mutate(
        window_id = paste(location_name, window$window_id, sep = "_"),  
        location_name = location_name,
        location_type = location_type,
        origin_lat = window$start_lat,
        origin_lon = window$start_lon,
        origin_date = window$start_date,
        origin_time = window$start_time
      )
    
    if (nrow(subsequent) > 0) {
      movement_list[[i]] <- subsequent
    }
  }
  
  return(bind_rows(movement_list))
}

# =============================================================================
# FUNCTION TO SAMPLE 15 WINDOWS PER OBSERVER
# =============================================================================

sample_valid_windows <- function(movement_data, n_sample = 15) {
  
  if (is.null(movement_data) || nrow(movement_data) == 0) return(NULL)
  
  # Get list of valid windows (those with movement)
  
  valid_windows <- movement_data %>%
    distinct(observer_id, window_id)
  
  # Sample up to 15 windows per observer
  
  sampled_window_ids <- valid_windows %>%
    group_by(observer_id) %>%
    group_split() %>%
    map_dfr(function(obs_windows) {
      n_to_sample <- min(nrow(obs_windows), n_sample)
      obs_windows %>% slice_sample(n = n_to_sample)
    })
  
  # Filter movement data to only sampled windows
  
  sampled_movement <- movement_data %>%
    semi_join(sampled_window_ids, by = c("observer_id", "window_id"))
  
  return(sampled_movement)
}

# =============================================================================
# LOOP THROUGH ALL STATES
# =============================================================================

states <- c("TX", "KS", "OK")
all_results <- list()

for (current_state in states) {
  
  cat("\n==============================================\n")
  cat("PROCESSING STATE:", current_state, "\n")
  cat("==============================================\n")
  
  # Load eBird data for this state
  
  cat("Loading eBird data...\n")
  ebd_file <- file.path(ebd_dir, paste0("ebd_US-", current_state, "_smp_relNov-2025.txt"))
  sampling_file <- file.path(sampling_dir, paste0("ebd_US-", current_state, "_smp_relNov-2025_sampling.txt"))
  
  temp_output <- file.path(output_dir, paste0("temp_", current_state, "_network.txt"))
  temp_sampling <- file.path(output_dir, paste0("temp_", current_state, "_network_sampling.txt"))
  
  auk_ebd(ebd_file, file_sampling = sampling_file) %>%
    auk_complete() %>%
    auk_date(date = date_range) %>%
    auk_filter(file = temp_output, 
               file_sampling = temp_sampling,
               overwrite = TRUE)
  
  ebd_state <- read_ebd(temp_output) %>%
    distinct(checklist_id, .keep_all = TRUE) %>%
    filter(is.na(effort_distance_km) | effort_distance_km <= 20) %>%
    mutate(
      observation_date = as.Date(observation_date),
      time = hms::as_hms(time_observations_started)
    )
  
  file.remove(temp_output)
  file.remove(temp_sampling)
  
  cat("Loaded", nrow(ebd_state), "checklists\n")
  
  # Filter locations to this state
  
  state_locations <- locations %>%
    filter(state_abbr == current_state)
  
  state_campuses <- state_locations %>% filter(location_type == "campus")
  state_hotspots <- state_locations %>% filter(location_type == "hotspot")
  
  cat("Processing", nrow(state_campuses), "campuses and", nrow(state_hotspots), "hotspots\n")
  
  state_results <- list()
  result_counter <- 1
  
  ##### Process Campuses #####
  
  cat("\n--- PROCESSING CAMPUSES ---\n")
  
  ebd_sf <- st_as_sf(ebd_state, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  
  for (i in 1:nrow(state_campuses)) {
    campus_name <- state_campuses$location_name[i]
    cat(sprintf("[%d/%d] %s\n", i, nrow(state_campuses), campus_name))
    
    # Get campus boundary
    
    campus_boundary <- campus_polygons %>%
      filter(inst_name == campus_name)
    
    if (nrow(campus_boundary) == 0) {
      cat("  × Campus polygon not found\n")
      next
    }
    
    # Spatial join
    
    campus_checklists <- st_join(ebd_sf, campus_boundary, join = st_within, left = FALSE) %>%
      st_drop_geometry() %>%
      select(-ends_with(".y")) %>%
      rename_with(~gsub("\\.x$", "", .), ends_with(".x"))
    
    if (nrow(campus_checklists) == 0) {
      cat("  × No checklists found\n")
      next
    }
    
    # Get top 10 observers
    
    top_observers <- get_top_observers(campus_checklists, n = 10)
    
    if (is.null(top_observers)) {
      cat("  × Fewer than 10 observers\n")
      next
    }
    
    cat("  ✓ Top 10 observers identified\n")
    
    # Get ALL 7-day windows
    
    all_windows <- get_all_windows(campus_checklists, top_observers)
    cat("  ✓ Found", nrow(all_windows), "total windows\n")
    
    # Track movement for ALL windows
    
    all_movement <- track_all_movement(all_windows, ebd_state, campus_name, "campus")
    
    if (is.null(all_movement) || nrow(all_movement) == 0) {
      cat("  × No trackable movement\n")
      next
    }
    
    n_valid_windows <- n_distinct(all_movement$window_id)
    cat("  ✓ Tracked", n_valid_windows, "windows with movement\n")
    
    # Sample up to 15 valid windows per observer
    
    sampled_movement <- sample_valid_windows(all_movement, n_sample = 15)
    
    n_sampled_windows <- n_distinct(sampled_movement$window_id)
    cat("  ✓ Sampled", n_sampled_windows, "windows,", nrow(sampled_movement), "total checklists\n")
    
    state_results[[result_counter]] <- sampled_movement
    result_counter <- result_counter + 1
  }
  
  rm(ebd_sf)
  gc(verbose = FALSE)
  
  ##### Process Hotspots #####
  
  cat("\n--- PROCESSING HOTSPOTS ---\n")
  
  for (i in 1:nrow(state_hotspots)) {
    hotspot_name <- state_hotspots$location_name[i]
    cat(sprintf("[%d/%d] %s\n", i, nrow(state_hotspots), hotspot_name))
    
    # Filter by locality
    
    hotspot_checklists <- ebd_state %>%
      filter(locality == hotspot_name)
    
    if (nrow(hotspot_checklists) == 0) {
      cat("  × No checklists found\n")
      next
    }
    
    # Get top 10 observers
    
    top_observers <- get_top_observers(hotspot_checklists, n = 10)
    
    if (is.null(top_observers)) {
      cat("  × Fewer than 10 observers\n")
      next
    }
    
    cat("  ✓ Top 10 observers identified\n")
    
    # Get ALL 7-day windows
    
    all_windows <- get_all_windows(hotspot_checklists, top_observers)
    cat("  ✓ Found", nrow(all_windows), "total windows\n")
    
    # Track movement for ALL windows
    
    all_movement <- track_all_movement(all_windows, ebd_state, hotspot_name, "hotspot")
    
    if (is.null(all_movement) || nrow(all_movement) == 0) {
      cat("  × No trackable movement\n")
      next
    }
    
    n_valid_windows <- n_distinct(all_movement$window_id)
    cat("  ✓ Tracked", n_valid_windows, "windows with movement\n")
    
    # Sample up to 15 valid windows per observer
    
    sampled_movement <- sample_valid_windows(all_movement, n_sample = 15)
    
    n_sampled_windows <- n_distinct(sampled_movement$window_id)
    cat("  ✓ Sampled", n_sampled_windows, "windows,", nrow(sampled_movement), "total checklists\n")
    
    state_results[[result_counter]] <- sampled_movement
    result_counter <- result_counter + 1
  }
  
  # Save state results
  
  state_output <- bind_rows(state_results)
  state_file <- file.path(output_dir, paste0("movement_network_", current_state, ".csv"))
  write.csv(state_output, state_file, row.names = FALSE)
  
  cat("\n✓ State complete:", nrow(state_output), "movements saved to", state_file, "\n")
  
  # Store for final combine
  
  all_results[[current_state]] <- state_output
  
  # Cleanup
  
  rm(ebd_state, state_locations, state_campuses, state_hotspots, state_results, state_output)
  gc(verbose = FALSE)
}

# =============================================================================
# COMBINE AND SAVE
# =============================================================================

# Combine state data

final_network <- bind_rows(all_results)

# Save data

final_file <- file.path(output_dir, "movement_network_ALL_locations.csv")
write.csv(final_network, final_file, row.names = FALSE)