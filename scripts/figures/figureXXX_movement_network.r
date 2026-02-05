##############################
#
# Movement Paths - DISTANCE LIMITED
# One observer per location, within 50 miles
#
##############################

library(tidyverse)
library(sf)
library(tigris)
library(ggplot2)
library(patchwork)

options(tigris_use_cache = TRUE)
setwd("/Users/ianbecker/Desktop/project_code/campus_community_science/data")

##############################
# LOAD DATA
##############################

tx_network <- read.csv("network_7day_tamu_blucher.csv")

tx_network <- tx_network %>%
  mutate(location_type = ifelse(origin == "Texas A&M", "Campus", "Hotspot"))

cat("Downloading Texas boundary from tigris...\n")
texas_sf <- states(cb = FALSE, year = 2021) %>%
  filter(NAME == "Texas") %>%
  st_transform(4326)

##############################
# CREATE SEQUENTIAL PATHS
##############################

observer_paths <- tx_network %>%
  arrange(observer_id, checklist_order) %>%
  group_by(observer_id, origin, location_type) %>%
  mutate(
    next_lat = lead(lat),
    next_lon = lead(lon),
    has_next = !is.na(next_lat)
  ) %>%
  ungroup()

# Pick ONE exemplar from each (most active)
top_campus <- observer_paths %>%
  filter(location_type == "Campus") %>%
  group_by(observer_id) %>%
  summarize(n_moves = sum(has_next, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(n_moves)) %>%
  slice(1)

top_hotspot <- observer_paths %>%
  filter(location_type == "Hotspot") %>%
  group_by(observer_id) %>%
  summarize(n_moves = sum(has_next, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(n_moves)) %>%
  slice(1)

cat("\n=== SELECTED OBSERVERS ===\n")
cat("Campus observer:", top_campus$observer_id, "with", top_campus$n_moves, "movements\n")
cat("Hotspot observer:", top_hotspot$observer_id, "with", top_hotspot$n_moves, "movements\n")

exemplar_paths <- observer_paths %>%
  filter(observer_id %in% c(top_campus$observer_id, top_hotspot$observer_id))

##############################
# FILTER BY DISTANCE FROM ORIGIN
##############################

# Calculate distance from starting point (rough approximation)
# 1 degree ≈ 111 km at equator, 69 miles
# For Texas (≈30°N): 1 degree longitude ≈ 96 km ≈ 60 miles

distance_limit_miles <- 50
distance_limit_degrees <- distance_limit_miles / 60  # Rough conversion

exemplar_paths <- exemplar_paths %>%
  mutate(
    # Calculate distance from start (Euclidean approximation)
    dist_from_start = sqrt((lon - start_lon)^2 + (lat - start_lat)^2),
    within_range = dist_from_start <= distance_limit_degrees
  )

# Summary before filtering
cat("\n=== DISTANCE FILTERING ===\n")
cat("Distance limit:", distance_limit_miles, "miles (~", round(distance_limit_degrees, 2), "degrees)\n")

distance_summary <- exemplar_paths %>%
  group_by(location_type) %>%
  summarize(
    total_locations = n(),
    within_range = sum(within_range),
    outside_range = sum(!within_range),
    pct_within = round(sum(within_range) / n() * 100, 1)
  )

print(distance_summary)

# Filter to only locations within range
exemplar_paths_filtered <- exemplar_paths %>%
  filter(within_range)

cat("\nAfter filtering to", distance_limit_miles, "miles:\n")
cat("Campus locations:", sum(exemplar_paths_filtered$location_type == "Campus"), "\n")
cat("Hotspot locations:", sum(exemplar_paths_filtered$location_type == "Hotspot"), "\n")

##############################
# UPDATE PATHS WITH FILTERED DATA
##############################

campus_data <- exemplar_paths_filtered %>% filter(location_type == "Campus")
hotspot_data <- exemplar_paths_filtered %>% filter(location_type == "Hotspot")

# Recalculate next locations after filtering
campus_data <- campus_data %>%
  arrange(checklist_order) %>%
  mutate(
    next_lat = lead(lat),
    next_lon = lead(lon),
    has_next = !is.na(next_lat)
  )

hotspot_data <- hotspot_data %>%
  arrange(checklist_order) %>%
  mutate(
    next_lat = lead(lat),
    next_lon = lead(lon),
    has_next = !is.na(next_lat)
  )

##############################
# COUNT VISITS PER LOCATION
##############################

campus_visits <- campus_data %>%
  group_by(lon, lat, locality_id) %>%
  summarize(
    n_visits = n(),
    is_start = any(checklist_order == 1),
    .groups = "drop"
  ) %>%
  mutate(point_type = ifelse(is_start, "Start", "Visited"))

hotspot_visits <- hotspot_data %>%
  group_by(lon, lat, locality_id) %>%
  summarize(
    n_visits = n(),
    is_start = any(checklist_order == 1),
    .groups = "drop"
  ) %>%
  mutate(point_type = ifelse(is_start, "Start", "Visited"))

##############################
# CALCULATE ZOOM BOUNDS
##############################

very_tight_buffer <- 0.25

# Campus
campus_lon_range <- range(campus_data$lon, na.rm = TRUE)
campus_lat_range <- range(campus_data$lat, na.rm = TRUE)

campus_bounds <- list(
  xmin = campus_lon_range[1] - very_tight_buffer,
  xmax = campus_lon_range[2] + very_tight_buffer,
  ymin = campus_lat_range[1] - very_tight_buffer,
  ymax = campus_lat_range[2] + very_tight_buffer
)

# Hotspot
hotspot_lon_range <- range(hotspot_data$lon, na.rm = TRUE)
hotspot_lat_range <- range(hotspot_data$lat, na.rm = TRUE)

hotspot_bounds <- list(
  xmin = hotspot_lon_range[1] - very_tight_buffer,
  xmax = hotspot_lon_range[2] + very_tight_buffer,
  ymin = hotspot_lat_range[1] - very_tight_buffer,
  ymax = hotspot_lat_range[2] + very_tight_buffer
)

##############################
# CAMPUS MAP
##############################

p_campus <- ggplot() +
  geom_sf(data = texas_sf, fill = "gray95", color = "gray50", linewidth = 0.3) +
  geom_segment(data = campus_data %>% filter(has_next),
               aes(x = lon, y = lat,
                   xend = next_lon, yend = next_lat),
               color = "#FFB703", alpha = 0.6, linewidth = 0.7,
               arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  geom_point(data = campus_visits,
             aes(x = lon, y = lat, size = n_visits, fill = point_type),
             color = "black", shape = 21, stroke = 0.8, alpha = 0.9) +
  scale_fill_manual(values = c("Start" = "#E63946", "Visited" = "#FFB703")) +
  scale_size_continuous(range = c(3, 10)) +
  coord_sf(
    xlim = c(campus_bounds$xmin, campus_bounds$xmax),
    ylim = c(campus_bounds$ymin, campus_bounds$ymax)
  ) +
  theme_void() +
  theme(
    text = element_text(size = 18),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(title = "Campus")

##############################
# HOTSPOT MAP
##############################

p_hotspot <- ggplot() +
  geom_sf(data = texas_sf, fill = "gray95", color = "gray50", linewidth = 0.3) +
  geom_segment(data = hotspot_data %>% filter(has_next),
               aes(x = lon, y = lat,
                   xend = next_lon, yend = next_lat),
               color = "#023047", alpha = 0.6, linewidth = 0.7,
               arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  geom_point(data = hotspot_visits,
             aes(x = lon, y = lat, size = n_visits, fill = point_type),
             color = "black", shape = 21, stroke = 0.8, alpha = 0.9) +
  scale_fill_manual(values = c("Start" = "#E63946", "Visited" = "#023047")) +
  scale_size_continuous(range = c(3, 10)) +
  coord_sf(
    xlim = c(hotspot_bounds$xmin, hotspot_bounds$xmax),
    ylim = c(hotspot_bounds$ymin, hotspot_bounds$ymax)
  ) +
  theme_void() +
  theme(
    text = element_text(size = 18),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(title = "Hotspot")

##############################
# COMBINE
##############################

p_combined <- p_campus | p_hotspot

ggsave("texas_movement_limited_50mi.png", p_combined, width = 14, height = 7, dpi = 300)

cat("\nMap saved: texas_movement_limited_50mi.png\n")
cat("Showing ONE observer per panel, within 50 miles of origin\n")



