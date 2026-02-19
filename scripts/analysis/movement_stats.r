##############################
# Movement Pattern Analysis
##############################

library(tidyverse)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)

# Load data
tx_network <- read.csv("network_7day_tamu_blucher.csv")

tx_network <- tx_network %>%
  mutate(location_type = ifelse(origin == "Texas A&M", "Campus", "Hotspot"))

##############################
# Split by location type
##############################

campus_data <- tx_network %>% filter(location_type == "Campus")
hotspot_data <- tx_network %>% filter(location_type == "Hotspot")

##############################
# 1. UNIQUE LOCATIONS & COUNTIES
##############################

cat("=== SPATIAL EXTENT ===\n")

# Campus
campus_locations <- n_distinct(campus_data$locality_id)
campus_counties <- n_distinct(campus_data$county)

cat("Campus observers:\n")
cat("  Unique locations:", campus_locations, "\n")
cat("  Counties visited:", campus_counties, "\n")

# Hotspot
hotspot_locations <- n_distinct(hotspot_data$locality_id)
hotspot_counties <- n_distinct(hotspot_data$county)

cat("Hotspot observers:\n")
cat("  Unique locations:", hotspot_locations, "\n")
cat("  Counties visited:", hotspot_counties, "\n\n")

##############################
# 2. DISTANCE CALCULATIONS
##############################

cat("=== DISTANCE PATTERNS ===\n")

# Calculate distance between consecutive points using Haversine
haversine <- function(lon1, lat1, lon2, lat2) {
  R <- 6371  # Earth radius in km
  
  dLat <- (lat2 - lat1) * pi / 180
  dLon <- (lon2 - lon1) * pi / 180
  
  a <- sin(dLat/2)^2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dLon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  
  return(R * c)
}

# Campus distances
campus_distances <- campus_data %>%
  arrange(observer_id, checklist_order) %>%
  group_by(observer_id) %>%
  mutate(
    next_lat = lead(lat),
    next_lon = lead(lon),
    distance_km = ifelse(!is.na(next_lat),
                         haversine(lon, lat, next_lon, next_lat),
                         NA)
  ) %>%
  ungroup()

campus_dist_stats <- campus_distances %>%
  filter(!is.na(distance_km)) %>%
  summarize(
    mean_dist = mean(distance_km),
    median_dist = median(distance_km),
    max_dist = max(distance_km),
    total_dist = sum(distance_km)
  )

# Hotspot distances
hotspot_distances <- hotspot_data %>%
  arrange(observer_id, checklist_order) %>%
  group_by(observer_id) %>%
  mutate(
    next_lat = lead(lat),
    next_lon = lead(lon),
    distance_km = ifelse(!is.na(next_lat),
                         haversine(lon, lat, next_lon, next_lat),
                         NA)
  ) %>%
  ungroup()

hotspot_dist_stats <- hotspot_distances %>%
  filter(!is.na(distance_km)) %>%
  summarize(
    mean_dist = mean(distance_km),
    median_dist = median(distance_km),
    max_dist = max(distance_km),
    total_dist = sum(distance_km)
  )

cat("Campus observers:\n")
cat("  Mean movement distance:", round(campus_dist_stats$mean_dist, 1), "km\n")
cat("  Median distance:", round(campus_dist_stats$median_dist, 1), "km\n")
cat("  Max single movement:", round(campus_dist_stats$max_dist, 1), "km\n")
cat("  Total distance covered:", round(campus_dist_stats$total_dist, 1), "km\n\n")

cat("Hotspot observers:\n")
cat("  Mean movement distance:", round(hotspot_dist_stats$mean_dist, 1), "km\n")
cat("  Median distance:", round(hotspot_dist_stats$median_dist, 1), "km\n")
cat("  Max single movement:", round(hotspot_dist_stats$max_dist, 1), "km\n")
cat("  Total distance covered:", round(hotspot_dist_stats$total_dist, 1), "km\n\n")

##############################
# 3. LOCATION TYPE BREAKDOWN
##############################

cat("=== LOCATION TYPE USAGE ===\n")

# Campus
campus_by_type <- campus_data %>%
  count(locality_type) %>%
  mutate(pct = n / sum(n) * 100)

cat("Campus observers visited:\n")
print(campus_by_type)
cat("\n")

# Hotspot
hotspot_by_type <- hotspot_data %>%
  count(locality_type) %>%
  mutate(pct = n / sum(n) * 100)

cat("Hotspot observers visited:\n")
print(hotspot_by_type)
cat("\n")

##############################
# 4. DISTANCE FROM ORIGIN DISTRIBUTION
##############################

cat("=== DISTANCE FROM ORIGIN ===\n")

# Distance from starting point
campus_from_origin <- campus_data %>%
  mutate(dist_from_origin = haversine(start_lon, start_lat, lon, lat)) %>%
  summarize(
    mean_from_origin = mean(dist_from_origin),
    median_from_origin = median(dist_from_origin),
    max_from_origin = max(dist_from_origin)
  )

hotspot_from_origin <- hotspot_data %>%
  mutate(dist_from_origin = haversine(start_lon, start_lat, lon, lat)) %>%
  summarize(
    mean_from_origin = mean(dist_from_origin),
    median_from_origin = median(dist_from_origin),
    max_from_origin = max(dist_from_origin)
  )

cat("Campus observers:\n")
cat("  Mean distance from campus:", round(campus_from_origin$mean_from_origin, 1), "km\n")
cat("  Median distance from campus:", round(campus_from_origin$median_from_origin, 1), "km\n")
cat("  Max distance from campus:", round(campus_from_origin$max_from_origin, 1), "km\n\n")

cat("Hotspot observers:\n")
cat("  Mean distance from hotspot:", round(hotspot_from_origin$mean_from_origin, 1), "km\n")
cat("  Median distance from hotspot:", round(hotspot_from_origin$median_from_origin, 1), "km\n")
cat("  Max distance from hotspot:", round(hotspot_from_origin$max_from_origin, 1), "km\n\n")

##############################
# 5. TEMPORAL PATTERNS
##############################

cat("=== TEMPORAL PATTERNS ===\n")

campus_temporal <- campus_data %>%
  group_by(observer_id) %>%
  summarize(
    unique_dates = n_distinct(date),
    checklists_per_day = n() / n_distinct(date),
    .groups = "drop"
  )

hotspot_temporal <- hotspot_data %>%
  group_by(observer_id) %>%
  summarize(
    unique_dates = n_distinct(date),
    checklists_per_day = n() / n_distinct(date),
    .groups = "drop"
  )

cat("Campus observers:\n")
cat("  Avg unique days birded:", round(mean(campus_temporal$unique_dates), 1), "\n")
cat("  Avg checklists per active day:", round(mean(campus_temporal$checklists_per_day), 1), "\n\n")

cat("Hotspot observers:\n")
cat("  Avg unique days birded:", round(mean(hotspot_temporal$unique_dates), 1), "\n")
cat("  Avg checklists per active day:", round(mean(hotspot_temporal$checklists_per_day), 1), "\n\n")

##############################
# SUMMARY FOR RESULTS
##############################

cat("=== SUMMARY FOR RESULTS SECTION ===\n")
cat("\nCampus observers (n = 50) submitted", nrow(campus_data), 
    "checklists across", campus_locations, "unique locations spanning",
    campus_counties, "counties, traveling an average of", 
    round(campus_dist_stats$mean_dist, 1), "km between sites.\n")

cat("\nHotspot observers (n = 50) submitted", nrow(hotspot_data), 
    "checklists across", hotspot_locations, "unique locations spanning",
    hotspot_counties, "counties, traveling an average of", 
    round(hotspot_dist_stats$mean_dist, 1), "km between sites.\n")

