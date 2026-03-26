##############################
#
# Figure 5: Observer Movement Networks (Campus vs Hotspot)
# Ian Becker
# March 2026
#
##############################

# This script creates tilted maps comparing an exemplar movement network of 
# a campus-based and hotspot-based observer. This makes Figure 5 in the main text

library(tidyverse)
library(sf)
library(tigris)
library(ggplot2)
library(patchwork)
library(layer)

options(tigris_use_cache = TRUE)

setwd("PATH HERE")
output_dir <- "PATH HERE"

# ============================================================================
# SETUP MAP TRANSFORMATIONS AND FUNCTIONS
# ============================================================================

# Visual tilt settings 

Y_TILT       <- 3        # vertical tile 
ANGLE_ROTATE <- pi / 20  # tile angle
X_STRETCH    <- 2        # horizontal stretch
Y_STRETCH    <- 1.2      # vertical stretch
X_TILT       <- 0        # horizontal tilt

# Spike and distance settings

SPIKE_FRAC   <- 0.18     
DIST_LIMIT_M <- 50 * 1000

# Build transformation settings for map

shear_mat  <- matrix(c(X_STRETCH, Y_STRETCH, X_TILT, Y_TILT), 2, 2)
rotate_mat <- matrix(c(cos(ANGLE_ROTATE),  sin(ANGLE_ROTATE),
                       -sin(ANGLE_ROTATE), cos(ANGLE_ROTATE)), 2, 2)
transform_mat <- shear_mat %*% rotate_mat

# Function to apply tilt transformation to lon/lat coordinates

tilt_coords <- function(df, lon_col = "lon", lat_col = "lat") {
  pts <- as.matrix(df[, c(lon_col, lat_col)])
  out <- pts %*% transform_mat
  df$x_t <- out[, 1]
  df$y_t <- out[, 2]
  df
}

# Function to convert tilted sf object to plain data frame for geom_polygon

sf_to_df <- function(sf_obj) {
  coords    <- st_coordinates(sf_obj)
  col_names <- colnames(coords)
  feat_col  <- col_names[length(col_names)]
  df        <- as.data.frame(coords)
  n_per_feat <- as.integer(table(df[[feat_col]]))
  df$NAME   <- rep(sf_obj$NAME, times = n_per_feat)
  if ("L2" %in% col_names) {
    df$group <- paste(df$L2, df[[feat_col]], sep = "_")
  } else {
    df$group <- as.character(df[[feat_col]])
  }
  df
}

# ============================================================================
# INITIAL DATA LOAD AND PREP
# ============================================================================

# Load in data

tx_network <- read.csv("network_7day_tamu_blucher.csv")

# Classify location type

tx_network <- tx_network %>%
  mutate(location_type = ifelse(
    grepl("Texas A.*M|TAMU", origin, ignore.case = TRUE), "Campus", "Hotspot"
  ))

# Load Texas county boundaries

texas_counties <- counties(state = "TX", cb = FALSE, year = 2021) %>%
  st_transform(4326)

# Extract origin coordinates for each observer

observer_origins <- tx_network %>%
  filter(checklist_order == 1) %>%
  select(observer_id, origin_lon = lon, origin_lat = lat) %>%
  st_as_sf(coords = c("origin_lon", "origin_lat"), crs = 4326)

# Calculate distance from each checklist to its observer's origin

tx_sf <- tx_network %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  left_join(
    observer_origins %>% st_drop_geometry() %>%
      bind_cols(origin_geom = st_geometry(observer_origins)),
    by = "observer_id"
  ) %>%
  rowwise() %>%
  mutate(dist_m = as.numeric(st_distance(geometry, origin_geom))) %>%
  ungroup()

tx_network <- tx_sf %>%
  mutate(lon = st_coordinates(.)[, 1],
         lat = st_coordinates(.)[, 2]) %>%
  st_drop_geometry()

# Find exemplar observer with most movement events from campus

top_campus_id <- tx_network %>%
  filter(location_type == "Campus", dist_m <= DIST_LIMIT_M) %>%
  group_by(observer_id) %>%
  summarize(n_moves = sum(checklist_order > 1), .groups = "drop") %>%
  arrange(desc(n_moves)) %>% slice(1) %>% pull(observer_id)

# Find exemplar observer with most movement events from hotspot

top_hotspot_id <- tx_network %>%
  filter(location_type == "Hotspot", dist_m <= DIST_LIMIT_M) %>%
  group_by(observer_id) %>%
  summarize(n_moves = sum(checklist_order > 1), .groups = "drop") %>%
  arrange(desc(n_moves)) %>% slice(1) %>% pull(observer_id)

# ============================================================================
# MOVEMENT DATA PREP
# ============================================================================

# Campus observer data

campus_data <- tx_network %>%
  filter(observer_id == top_campus_id, dist_m <= DIST_LIMIT_M) %>%
  arrange(checklist_order) %>%
  mutate(next_lat = lead(lat), next_lon = lead(lon),
         has_next = !is.na(next_lat))

# Hotspot observer data

hotspot_data <- tx_network %>%
  filter(observer_id == top_hotspot_id, dist_m <= DIST_LIMIT_M) %>%
  arrange(checklist_order) %>%
  mutate(next_lat = lead(lat), next_lon = lead(lon),
         has_next = !is.na(next_lat))

# Get origin locality IDs

campus_origin_loc  <- campus_data  %>% filter(checklist_order == 1) %>% pull(locality_id)
hotspot_origin_loc <- hotspot_data %>% filter(checklist_order == 1) %>% pull(locality_id)

# Aggregate visits per location

campus_visits <- campus_data %>%
  group_by(lon, lat, locality_id) %>%
  summarize(n_visits = n(), .groups = "drop") %>%
  mutate(point_type = ifelse(locality_id == campus_origin_loc, "Start", "Visited"))

hotspot_visits <- hotspot_data %>%
  group_by(lon, lat, locality_id) %>%
  summarize(n_visits = n(), .groups = "drop") %>%
  mutate(point_type = ifelse(locality_id == hotspot_origin_loc, "Start", "Visited"))

cat("✓ Movement data prepared\n")

# Convert visits to sf

campus_pts_sf  <- st_as_sf(campus_visits,  coords = c("lon","lat"), crs = 4326)
hotspot_pts_sf <- st_as_sf(hotspot_visits, coords = c("lon","lat"), crs = 4326)

# Spatial join to find counties containing movement points

campus_county_names  <- st_join(campus_pts_sf,  texas_counties) %>%
  st_drop_geometry() %>% distinct(NAME) %>% pull(NAME)
hotspot_county_names <- st_join(hotspot_pts_sf, texas_counties) %>%
  st_drop_geometry() %>% distinct(NAME) %>% pull(NAME)

# Filter counties

campus_counties  <- texas_counties %>% filter(NAME %in% campus_county_names)
hotspot_counties <- texas_counties %>% filter(NAME %in% hotspot_county_names)

# Identify main county (contains origin point)

campus_main_county <- st_join(
  campus_visits %>% filter(point_type == "Start") %>%
    st_as_sf(coords = c("lon","lat"), crs = 4326), texas_counties
) %>% st_drop_geometry() %>% slice(1) %>% pull(NAME)

hotspot_main_county <- st_join(
  hotspot_visits %>% filter(point_type == "Start") %>%
    st_as_sf(coords = c("lon","lat"), crs = 4326), texas_counties
) %>% st_drop_geometry() %>% slice(1) %>% pull(NAME)

# ============================================================================
# PREP TILT TRANSFORMATION FOR MAP
# ============================================================================

# Tilting counties

campus_counties_t  <- tilt_map(campus_counties,
                               y_tilt = Y_TILT, angle_rotate = ANGLE_ROTATE,
                               x_stretch = X_STRETCH, y_stretch = Y_STRETCH)
hotspot_counties_t <- tilt_map(hotspot_counties,
                               y_tilt = Y_TILT, angle_rotate = ANGLE_ROTATE,
                               x_stretch = X_STRETCH, y_stretch = Y_STRETCH)

campus_counties_df  <- sf_to_df(campus_counties_t)
hotspot_counties_df <- sf_to_df(hotspot_counties_t)

# Tilting movement segments

campus_segs_t <- campus_data %>%
  filter(has_next) %>%
  tilt_coords("lon", "lat") %>% rename(x_from = x_t, y_from = y_t) %>%
  tilt_coords("next_lon", "next_lat") %>% rename(x_to = x_t, y_to = y_t)

hotspot_segs_t <- hotspot_data %>%
  filter(has_next) %>%
  tilt_coords("lon", "lat") %>% rename(x_from = x_t, y_from = y_t) %>%
  tilt_coords("next_lon", "next_lat") %>% rename(x_to = x_t, y_to = y_t)

# Tilting visit points

campus_visits_t  <- tilt_coords(campus_visits)
hotspot_visits_t <- tilt_coords(hotspot_visits)

# Function to create 3D point data 

build_spikes_t <- function(visits_t, counties_df, frac = SPIKE_FRAC, max_v_override = NULL) {
  bbox_h <- diff(range(counties_df$Y))
  max_h  <- bbox_h * frac
  max_v  <- if (!is.null(max_v_override)) max_v_override else max(visits_t$n_visits)
  visits_t %>%
    mutate(
      spike_h  = (n_visits / max_v) * max_h,
      x_base   = x_t,
      y_base   = y_t,
      x_top    = x_t,
      y_top    = y_t + spike_h
    )
}

# Use shared max for consistent scaling

global_max_v    <- max(campus_visits_t$n_visits, hotspot_visits_t$n_visits)
global_max_size <- global_max_v

campus_spikes  <- build_spikes_t(campus_visits_t,  campus_counties_df,  max_v_override = global_max_v)
hotspot_spikes <- build_spikes_t(hotspot_visits_t, hotspot_counties_df, max_v_override = global_max_v)

# ============================================================================
# PANEL PLOTTING FUNCTION
# ============================================================================

make_panel <- function(counties_df, main_county_name,
                       segs_t, spikes, line_color, title_label,
                       size_max = 1, show_legend = FALSE) {
  
  main_df <- counties_df %>% filter(NAME == main_county_name)
  
  # Calculate plot bounds from polygon vertices and spike tops
  
  all_poly_x <- counties_df$X
  all_poly_y <- counties_df$Y
  map_w <- diff(range(all_poly_x))
  map_h <- diff(range(all_poly_y))
  
  xlim <- c(min(all_poly_x) - map_w * 0.06,
            max(all_poly_x) + map_w * 0.06)
  ylim <- c(min(all_poly_y) - map_h * 0.06,
            max(spikes$y_top) + map_h * 0.30)
  
  # Scale bar precomputation (100 km = 4 blocks of 25 km)
  
  sb_deg_total <- 0.90  # ~100 km at 30°N
  sb_n_blocks  <- 4
  sb_block_len <- (sb_deg_total * transform_mat[1, 1]) / sb_n_blocks
  sb_x0        <- min(all_poly_x) + map_w * 0.05
  sb_y0        <- min(all_poly_y) - map_h * 0.12
  sb_bar_h     <- map_h * 0.018
  
  # Alternating filled blocks
  
  sb_blocks <- data.frame(
    xmin  = sb_x0 + (0:(sb_n_blocks-1)) * sb_block_len,
    xmax  = sb_x0 + (1:sb_n_blocks)     * sb_block_len,
    ymin  = sb_y0,
    ymax  = sb_y0 + sb_bar_h,
    fill  = ifelse((0:(sb_n_blocks-1)) %% 2 == 0, "black", "white")
  )
  
  # Tick labels
  
  sb_labels <- data.frame(
    x     = sb_x0 + (0:sb_n_blocks) * sb_block_len,
    y     = sb_y0 - map_h * 0.018,
    label = c("0", "25", "50", "75", "100 km")
  )
  
  # Build plot
  
  ggplot() +
    
    # Base map (tilted polygons)
    
    geom_polygon(data = counties_df,
                 aes(x = X, y = Y, group = group),
                 fill = "gray93", color = "gray60", linewidth = 0.4) +
    geom_polygon(data = main_df,
                 aes(x = X, y = Y, group = group),
                 fill = "gray85", color = "black", linewidth = 1.0) +
    
    # Floor connection lines
    
    geom_segment(data = segs_t,
                 aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
                 color = line_color, alpha = 0.55, linewidth = 0.7) +
    
    # Point stems
    
    geom_segment(data = spikes,
                 aes(x = x_base, y = y_base,
                     xend = x_top, yend = y_top,
                     color = point_type),
                 linewidth = 1.1, alpha = 0.9) +
    scale_color_manual(values = c("Start"   = "#E63946",
                                  "Visited" = line_color),
                       guide = "none") +
    
    # 3D points at top of stems
    
    geom_point(data = spikes,
               aes(x = x_top, y = y_top,
                   size = n_visits, fill = point_type),
               shape = 21, color = "white", stroke = 0.5, alpha = 0.95) +
    scale_fill_manual(values = c("Start"   = "#E63946",
                                 "Visited" = line_color),
                      guide = "none") +
    scale_size_continuous(
      range  = c(2, 8),
      limits = c(1, size_max),
      breaks = pretty(c(1, size_max), n = 4),
      name   = "Visits",
      guide  = if (show_legend) guide_legend(override.aes = list(shape = 21,
                                                                 fill  = "gray50",
                                                                 color = "white")) 
      else "none"
    ) +
    
    # Footprint dots
    
    geom_point(data = spikes,
               aes(x = x_base, y = y_base),
               size = 1.2, color = "gray30", alpha = 0.5) +
    
    # Scale bar
    
    geom_rect(data = sb_blocks,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = I(sb_blocks$fill),
              color = "black", linewidth = 0.4, inherit.aes = FALSE) +
    geom_text(data = sb_labels,
              aes(x = x, y = y, label = label),
              size = 2.8, vjust = 1, inherit.aes = FALSE) +
    
    coord_fixed(ratio = 1, xlim = xlim, ylim = ylim, expand = FALSE, clip = "off") +
    theme_void() +
    theme(
      legend.position = if (show_legend) "right" else "none",
      legend.title    = element_text(size = 9),
      legend.text     = element_text(size = 8),
      plot.margin     = margin(5, 5, 5, 5),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    labs(title = NULL)
}

# ============================================================================
# PLOT MAP PANELS
# ============================================================================

# Campus map panel

p_campus <- make_panel(
  campus_counties_df,  campus_main_county,
  campus_segs_t,  campus_spikes,
  "#FFB703", "Campus",
  size_max = global_max_size, show_legend = FALSE
)

# Hotspot map panel 

p_hotspot <- make_panel(
  hotspot_counties_df, hotspot_main_county,
  hotspot_segs_t, hotspot_spikes,
  "#023047", "Hotspot",
  size_max = global_max_size, show_legend = TRUE
)

# Combine panels

p_combined <- p_campus + p_hotspot +
  plot_layout(widths = c(1, 1))

# Save figure

ggsave(
  filename = file.path(output_dir, "Figure_5_movement_network.png"),
  plot     = p_combined,
  width    = 14, height = 8, dpi = 300, bg = "white"
)
