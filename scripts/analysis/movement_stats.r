##############################
#
# Movement Pattern Analysis 
# Ian Becker
# April 2026
#
##############################

# This script is used to analyze movement patterns of observers
# at hotspots and campuses. The original dataset containing movements
# was not included due to privacy concerns, but this script can be run on 
# the example included dataset "movement_network_EXAMPLE.csv" to reproduce the analysis

library(tidyverse)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)

setwd("PATH HERE")

# ============================================================================
# LOAD AND PROCESS DATA
# ============================================================================

# Load in movement data

network_data <- read.csv("movement_network_EXAMPLE.csv")

# Create checklist order within each window

network_data <- network_data %>%
  mutate(observation_date = as.Date(observation_date)) %>%
  arrange(observer_id, window_id, observation_date, time) %>%
  group_by(observer_id, window_id) %>%
  mutate(checklist_order = row_number()) %>%
  ungroup()

# Add county information via spatial join

cat("Adding county information...\n")

# Get state boundaries

states <- c("TX", "KS", "OK")
counties_list <- map(states, ~counties(state = .x, cb = TRUE))
all_counties <- do.call(rbind, counties_list) %>%
  st_transform(4326) %>%
  select(county = NAME, state = STUSPS)

# Convert to sf

network_sf <- st_as_sf(network_data, 
                       coords = c("longitude", "latitude"), 
                       crs = 4326, 
                       remove = FALSE)

# Spatial join

network_with_county <- st_join(network_sf, all_counties, join = st_within) %>%
  st_drop_geometry()

# Divide by location type

campus_data <- network_with_county %>% filter(location_type == "campus")
hotspot_data <- network_with_county %>% filter(location_type == "hotspot")


# ============================================================================
# BASE SUMMARY
# ============================================================================

# Unique locations

n_distinct(campus_data$locality_id)
n_distinct(hotspot_data$locality_id)

# Total observers

n_distinct(campus_data$observer_id)
n_distinct(hotspot_data$observer_id)

# Total movement windows

n_distinct(campus_data$window_id)
n_distinct(hotspot_data$window_id)

# ============================================================================
# DISTANCE EQUATION AND LOCATION DISTANCES
# ============================================================================

# Calculate haversine distance function

haversine <- function(lon1, lat1, lon2, lat2) {
  R <- 6371  
  
  dLat <- (lat2 - lat1) * pi / 180
  dLon <- (lon2 - lon1) * pi / 180
  
  a <- sin(dLat/2)^2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dLon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  
  return(R * c)
}

# Campus consecutive distances

campus_distances <- campus_data %>%
  arrange(observer_id, window_id, checklist_order) %>%
  group_by(observer_id, window_id) %>%
  mutate(
    next_lat = lead(latitude),
    next_lon = lead(longitude),
    distance_km = ifelse(!is.na(next_lat),
                         haversine(longitude, latitude, next_lon, next_lat),
                         NA)
  ) %>%
  ungroup()

campus_dist_stats <- campus_distances %>%
  filter(!is.na(distance_km), distance_km > 0) %>%  # Remove 0km movements
  summarize(
    mean_dist = mean(distance_km),
    median_dist = median(distance_km),
    max_dist = max(distance_km),
    total_dist = sum(distance_km),
    n_movements = n()
  )

# Hotspot consecutive distances

hotspot_distances <- hotspot_data %>%
  arrange(observer_id, window_id, checklist_order) %>%
  group_by(observer_id, window_id) %>%
  mutate(
    next_lat = lead(latitude),
    next_lon = lead(longitude),
    distance_km = ifelse(!is.na(next_lat),
                         haversine(longitude, latitude, next_lon, next_lat),
                         NA)
  ) %>%
  ungroup()

hotspot_dist_stats <- hotspot_distances %>%
  filter(!is.na(distance_km), distance_km > 0) %>%
  summarize(
    mean_dist = mean(distance_km),
    median_dist = median(distance_km),
    max_dist = max(distance_km),
    total_dist = sum(distance_km),
    n_movements = n()
  )

# ============================================================================
# DISTANCE FROM ORIGIN
# ============================================================================

# Campus distance from origin

campus_from_origin <- campus_data %>%
  mutate(dist_from_origin = haversine(origin_lon, origin_lat, longitude, latitude)) %>%
  summarize(
    mean_from_origin = mean(dist_from_origin),
    median_from_origin = median(dist_from_origin),
    max_from_origin = max(dist_from_origin),
    q25 = quantile(dist_from_origin, 0.25),
    q75 = quantile(dist_from_origin, 0.75)
  )

# Hotspot distance from origin

hotspot_from_origin <- hotspot_data %>%
  mutate(dist_from_origin = haversine(origin_lon, origin_lat, longitude, latitude)) %>%
  summarize(
    mean_from_origin = mean(dist_from_origin),
    median_from_origin = median(dist_from_origin),
    max_from_origin = max(dist_from_origin),
    q25 = quantile(dist_from_origin, 0.25),
    q75 = quantile(dist_from_origin, 0.75)
  )

# ============================================================================
# RETURN RATE TO ORIGIN 
# ============================================================================

# Campus return to origin

campus_returns <- campus_data %>%
  group_by(window_id) %>%
  summarize(
    origin_location = first(location_name),
    revisited_origin = any(locality == origin_location & checklist_order > 1),
    .groups = "drop"
  ) %>%
  summarize(
    total_windows = n(),
    windows_with_return = sum(revisited_origin),
    pct_return = sum(revisited_origin) / n() * 100
  )

# Hotspot return to origin

hotspot_returns <- hotspot_data %>%
  group_by(window_id) %>%
  summarize(
    origin_location = first(location_name),
    revisited_origin = any(locality == origin_location & checklist_order > 1),
    .groups = "drop"
  ) %>%
  summarize(
    total_windows = n(),
    windows_with_return = sum(revisited_origin),
    pct_return = sum(revisited_origin) / n() * 100
  )



# ============================================================================
# CHECKLISTS PER WINDOW 
# ============================================================================

# Campus checklists per window

campus_activity <- campus_data %>%
  group_by(window_id) %>%
  summarize(checklists = n(), .groups = "drop") %>%
  summarize(
    mean_checklists = mean(checklists),
    median_checklists = median(checklists),
    max_checklists = max(checklists)
  )

# Hotspot checklists per window

hotspot_activity <- hotspot_data %>%
  group_by(window_id) %>%
  summarize(checklists = n(), .groups = "drop") %>%
  summarize(
    mean_checklists = mean(checklists),
    median_checklists = median(checklists),
    max_checklists = max(checklists)
  )

# ============================================================================
# CONCENTRATION OF CHECKLISTS AT ORIGIN
# ============================================================================

# Campus concentration at origin

campus_concentration <- campus_data %>%
  group_by(window_id) %>%
  summarize(
    origin_name = first(location_name),
    total_checklists = n(),
    origin_checklists = sum(locality == origin_name),
    pct_at_origin = origin_checklists / total_checklists * 100,
    .groups = "drop"
  ) %>%
  summarize(
    mean_pct = mean(pct_at_origin),
    median_pct = median(pct_at_origin)
  )

# Hotspot concentration at origin

hotspot_concentration <- hotspot_data %>%
  group_by(window_id) %>%
  summarize(
    origin_name = first(location_name),
    total_checklists = n(),
    origin_checklists = sum(locality == origin_name),
    pct_at_origin = origin_checklists / total_checklists * 100,
    .groups = "drop"
  ) %>%
  summarize(
    mean_pct = mean(pct_at_origin),
    median_pct = median(pct_at_origin)
  )

