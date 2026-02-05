##############################
#
# Movement Paths - ONLY COUNTIES WITH DATA
#
##############################

library(tidyverse)
library(sf)
library(tigris)
library(ggplot2)
library(patchwork)

options(tigris_use_cache = TRUE)
setwd("/Users/ianbecker/Desktop/project_code/campus_community_science/data")
output_dir <- "/Users/ianbecker/Desktop/project_code/campus_community_science/figures_tables"

##############################
# LOAD DATA
##############################

tx_network <- read.csv("network_7day_tamu_blucher.csv")

tx_network <- tx_network %>%
  mutate(location_type = ifelse(origin == "Texas A&M", "Campus", "Hotspot"))

##############################
# GET COUNTY BOUNDARIES
##############################

cat("Downloading Texas county boundaries from tigris...\n")
texas_counties <- counties(state = "TX", cb = FALSE, year = 2021) %>%
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

# Pick ONE exemplar from each
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

exemplar_paths <- observer_paths %>%
  filter(observer_id %in% c(top_campus$observer_id, top_hotspot$observer_id))

##############################
# DISTANCE FILTER
##############################

distance_limit_miles <- 50
distance_limit_degrees <- distance_limit_miles / 60

exemplar_paths <- exemplar_paths %>%
  mutate(
    dist_from_start = sqrt((lon - start_lon)^2 + (lat - start_lat)^2),
    within_range = dist_from_start <= distance_limit_degrees
  ) %>%
  filter(within_range)

campus_data <- exemplar_paths %>% filter(location_type == "Campus")
hotspot_data <- exemplar_paths %>% filter(location_type == "Hotspot")

# Recalculate next locations
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
# IDENTIFY COUNTIES WITH CHECKLISTS
##############################

# Convert checklist locations to sf points
campus_points_sf <- st_as_sf(campus_data, coords = c("lon", "lat"), crs = 4326)
hotspot_points_sf <- st_as_sf(hotspot_data, coords = c("lon", "lat"), crs = 4326)

# Find which counties contain checklists
campus_counties_with_data <- st_join(campus_points_sf, texas_counties) %>%
  st_drop_geometry() %>%
  distinct(NAME) %>%
  pull(NAME)

hotspot_counties_with_data <- st_join(hotspot_points_sf, texas_counties) %>%
  st_drop_geometry() %>%
  distinct(NAME) %>%
  pull(NAME)

cat("\nCampus: Checklists in", length(campus_counties_with_data), "counties:", 
    paste(campus_counties_with_data, collapse = ", "), "\n")
cat("Hotspot: Checklists in", length(hotspot_counties_with_data), "counties:", 
    paste(hotspot_counties_with_data, collapse = ", "), "\n")

# Filter counties to only those with data
campus_counties <- texas_counties %>%
  filter(NAME %in% campus_counties_with_data)

hotspot_counties <- texas_counties %>%
  filter(NAME %in% hotspot_counties_with_data)

# Identify the main county (where origin is)
campus_origin <- campus_data %>% 
  filter(checklist_order == 1) %>%
  st_as_sf(coords = c("start_lon", "start_lat"), crs = 4326)

hotspot_origin <- hotspot_data %>% 
  filter(checklist_order == 1) %>%
  st_as_sf(coords = c("start_lon", "start_lat"), crs = 4326)

campus_main_county <- st_join(campus_origin, texas_counties) %>%
  st_drop_geometry() %>%
  pull(NAME)

hotspot_main_county <- st_join(hotspot_origin, texas_counties) %>%
  st_drop_geometry() %>%
  pull(NAME)

##############################
# COUNT VISITS
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
# BOUNDING BOX FROM COUNTIES WITH DATA
##############################

campus_bbox <- st_bbox(campus_counties)
hotspot_bbox <- st_bbox(hotspot_counties)

##############################
# CAMPUS MAP - SMALLER POINTS
##############################

p_campus <- ggplot() +
  # All counties with data
  geom_sf(data = campus_counties, fill = "gray95", color = "gray60", linewidth = 0.6) +
  # Highlight main county with darker border
  geom_sf(data = campus_counties %>% filter(NAME == campus_main_county), 
          fill = NA, color = "black", linewidth = 1.2) +
  # Movement arrows
  geom_segment(data = campus_data %>% filter(has_next),
               aes(x = lon, y = lat,
                   xend = next_lon, yend = next_lat),
               color = "#FFB703", alpha = 0.7, linewidth = 0.8,
               arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  # Points sized by visits (SMALLER)
  geom_point(data = campus_visits,
             aes(x = lon, y = lat, size = n_visits, fill = point_type),
             color = "black", shape = 21, stroke = 0.8, alpha = 0.95) +
  scale_fill_manual(values = c("Start" = "#E63946", "Visited" = "#FFB703")) +
  scale_size_continuous(range = c(2.5, 7)) +  # Much smaller range
  coord_sf(
    xlim = c(campus_bbox["xmin"], campus_bbox["xmax"]),
    ylim = c(campus_bbox["ymin"], campus_bbox["ymax"])
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
# HOTSPOT MAP - SMALLER POINTS
##############################

p_hotspot <- ggplot() +
  # All counties with data
  geom_sf(data = hotspot_counties, fill = "gray95", color = "gray60", linewidth = 0.6) +
  # Highlight main county with darker border
  geom_sf(data = hotspot_counties %>% filter(NAME == hotspot_main_county), 
          fill = NA, color = "black", linewidth = 1.2) +
  # Movement arrows
  geom_segment(data = hotspot_data %>% filter(has_next),
               aes(x = lon, y = lat,
                   xend = next_lon, yend = next_lat),
               color = "#023047", alpha = 0.7, linewidth = 0.8,
               arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  # Points sized by visits (SMALLER)
  geom_point(data = hotspot_visits,
             aes(x = lon, y = lat, size = n_visits, fill = point_type),
             color = "black", shape = 21, stroke = 0.8, alpha = 0.95) +
  scale_fill_manual(values = c("Start" = "#E63946", "Visited" = "#023047")) +
  scale_size_continuous(range = c(2.5, 7)) +  # Much smaller range
  coord_sf(
    xlim = c(hotspot_bbox["xmin"], hotspot_bbox["xmax"]),
    ylim = c(hotspot_bbox["ymin"], hotspot_bbox["ymax"])
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

ggsave(path = output_dir, "texas_movement_data_counties_only.png", p_combined, width = 14, height = 7, dpi = 300)

cat("\nMap saved: texas_movement_data_counties_only.png\n")
cat("Showing ONLY counties with checklist activity\n")
cat("Point size range: 2.5-7 (smaller)\n")

##############################
# ADD TEXAS COUNTY OVERVIEW MAPS
##############################

# Campus county map
texas_counties_campus <- texas_counties %>%
  mutate(has_data = ifelse(NAME %in% campus_counties_with_data, "Campus Data", "Other"))

p_campus_overview <- ggplot(texas_counties_campus) +
  geom_sf(aes(fill = has_data), color = "black", linewidth = 0.5) +
  scale_fill_manual(
    values = c("Campus Data" = "#FFB703", "Other" = "gray95"),
    name = ""
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(title = "Campus Network Counties")

ggsave(path = output_dir, "texas_counties_campus.png", p_campus_overview, width = 10, height = 8, dpi = 300)

# Hotspot county map
texas_counties_hotspot <- texas_counties %>%
  mutate(has_data = ifelse(NAME %in% hotspot_counties_with_data, "Hotspot Data", "Other"))

p_hotspot_overview <- ggplot(texas_counties_hotspot) +
  geom_sf(aes(fill = has_data), color = "black", linewidth = 0.5) +
  scale_fill_manual(
    values = c("Hotspot Data" = "#023047", "Other" = "gray95"),
    name = ""
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(title = "Hotspot Network Counties")

ggsave(path = output_dir, "texas_counties_hotspot.png", p_hotspot_overview, width = 10, height = 8, dpi = 300)

cat("\nCounty overview maps saved!\n")
