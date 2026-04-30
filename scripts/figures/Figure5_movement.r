##############################
#
# Figure 5: Movement Analysis
# Ian Becker
# April 2026
#
##############################

# This script is used to recreate all 4 panels in Figure 5
# in the main text. Subsequent editing was done after figure creation.
# To protect the privacy of those included in our study, we do not
# include the data to make this figure with the submission data. 

library(tidyverse)
library(sf)
library(tigris)
library(ggplot2)
library(layer)

options(tigris_use_cache = TRUE)

setwd("PATH HERE")
output_dir <- "PATH HERE"

# ============================================================================
# SHARED SETTINGS
# ============================================================================

campus_color  <- "#FFB703"
hotspot_color <- "#023047"
origin_color  <- "#E63946"

campus_median_km  <-  9.3
hotspot_median_km <- 27.9

# Tilt parameters

y_tilt       <- 3
angle_rotate <- pi / 20
x_stretch    <- 2
y_stretch    <- 1.2
x_tilt       <- 0

# Spike parameters

spike_frac  <- 0.4    # visited location spike height as fraction of map bbox
origin_frac <- 0.35   # origin spike height as fraction of map bbox

shear_mat     <- matrix(c(x_stretch, y_stretch, x_tilt, y_tilt), 2, 2)
rotate_mat    <- matrix(c(cos(angle_rotate),  sin(angle_rotate),
                          -sin(angle_rotate), cos(angle_rotate)), 2, 2)
transform_mat <- shear_mat %*% rotate_mat

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Apply tilt transform to lon/lat columns

tilt_coords <- function(df, lon_col = "lon", lat_col = "lat") {
  pts <- as.matrix(df[, c(lon_col, lat_col)])
  out <- pts %*% transform_mat
  df$x_t <- out[, 1]
  df$y_t <- out[, 2]
  df
}

# Convert tilted sf object to plain data frame for geom_polygon

sf_to_df <- function(sf_obj) {
  coords     <- st_coordinates(sf_obj)
  col_names  <- colnames(coords)
  feat_col   <- col_names[length(col_names)]
  df         <- as.data.frame(coords)
  n_per_feat <- as.integer(table(df[[feat_col]]))
  df$NAME    <- rep(sf_obj$NAME, times = n_per_feat)
  df$group   <- if ("L2" %in% col_names)
    paste(df$L2, df[[feat_col]], sep = "_")
  else
    as.character(df[[feat_col]])
  df
}

# Haversine distance in meters

haversine_m <- function(lon1, lat1, lon2, lat2) {
  R    <- 6371000
  phi1 <- lat1 * pi / 180
  phi2 <- lat2 * pi / 180
  dphi <- (lat2 - lat1) * pi / 180
  dlam <- (lon2 - lon1) * pi / 180
  a    <- sin(dphi/2)^2 + cos(phi1) * cos(phi2) * sin(dlam/2)^2
  2 * R * atan2(sqrt(a), sqrt(1 - a))
}

# Generate radius ring in tilted space

make_radius_ring_t <- function(origin_lon, origin_lat, radius_km, n = 200) {
  lat_deg <- radius_km / 111.0
  lon_deg <- radius_km / (111.0 * cos(origin_lat * pi / 180))
  theta   <- seq(0, 2 * pi, length.out = n)
  tilt_coords(data.frame(
    lon = origin_lon + lon_deg * cos(theta),
    lat = origin_lat + lat_deg * sin(theta)
  ), "lon", "lat")
}

# Build spike data from tilted visits

build_spikes_t <- function(visits_t, counties_df,
                           frac = spike_frac, max_v = NULL,
                           origin_frac = origin_frac) {
  bbox_h <- diff(range(counties_df$Y))
  if (is.null(max_v)) max_v <- max(visits_t$n_visits)
  visits_t %>%
    mutate(
      spike_h = ifelse(point_type == "Start",
                       bbox_h * origin_frac,
                       (n_visits / max_v) * bbox_h * frac),
      x_base  = x_t,
      y_base  = y_t,
      x_top   = x_t,
      y_top   = y_t + spike_h
    )
}

# Build scale bar data for movement maps

make_scalebar <- function(counties_df, map_w, map_h) {
  sb_block_len <- (0.90 * transform_mat[1, 1]) / 4
  sb_x0 <- min(counties_df$X) + map_w * 0.05
  sb_y0 <- min(counties_df$Y) - map_h * 0.12
  sb_h  <- map_h * 0.018
  list(
    blocks = data.frame(
      xmin = sb_x0 + (0:3) * sb_block_len,
      xmax = sb_x0 + (1:4) * sb_block_len,
      ymin = sb_y0, ymax = sb_y0 + sb_h,
      fill = ifelse((0:3) %% 2 == 0, "black", "white")
    ),
    labels = data.frame(
      x     = sb_x0 + (0:4) * sb_block_len,
      y     = sb_y0 - map_h * 0.018,
      label = c("0", "25", "50", "75", "100 km")
    )
  )
}

# ============================================================================
# LOAD AND PREP DATA
# ============================================================================

# Full dataset for violin (Panel A)

full_network <- read.csv("movement_network_EXAMPLE.csv")

# Prep Violin data

violin_data <- full_network %>%
  mutate(dist_km = haversine_m(longitude, latitude,
                               origin_lon, origin_lat) / 1000) %>%
  filter(dist_km <= 50, dist_km > 0) %>%
  select(location_type, dist_km)

# Network CSV for movement maps (Panels C and D)

tx_network <- read.csv("network_7day_CAMPUS_HOTSPOT.csv") %>%
  mutate(location_type = ifelse(
    grepl("Texas A.*M|TAMU", origin, ignore.case = TRUE),
    "Campus", "Hotspot"
  ))

texas_counties <- counties(state = "TX", cb = FALSE, year = 2021) %>%
  st_transform(4326)

# Exemplar window selection for campus map — find observer/visit with median

campus_best <- tx_network %>%
  filter(location_type == "Campus") %>%
  mutate(dist_m = haversine_m(lon, lat, start_lon, start_lat)) %>%
  group_by(observer_id, visit_id) %>%
  summarize(
    n_unique_loc = n_distinct(locality_id),
    median_dist  = median(dist_m, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  filter(n_unique_loc >= 8) %>%
  mutate(diff = abs(median_dist - campus_median_km * 1000)) %>%
  arrange(diff) %>% slice(1)

top_campus_id    <- campus_best$observer_id
top_campus_visit <- campus_best$visit_id

campus_obs <- tx_network %>%
  filter(observer_id == top_campus_id) %>%
  mutate(dist_m = haversine_m(lon, lat, start_lon, start_lat))

# Exemplar window selection for hotspot map — find observer/visit with median

hotspot_best <- tx_network %>%
  filter(location_type == "Hotspot") %>%
  mutate(dist_m = haversine_m(lon, lat, start_lon, start_lat)) %>%
  group_by(observer_id, visit_id) %>%
  summarize(
    n_unique_loc = n_distinct(locality_id),
    median_dist  = median(dist_m, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  filter(n_unique_loc >= 8) %>%
  mutate(diff = abs(median_dist - hotspot_median_km * 1000)) %>%
  arrange(diff) %>% slice(1)

top_hotspot_id    <- hotspot_best$observer_id
top_hotspot_visit <- hotspot_best$visit_id

hotspot_obs <- tx_network %>%
  filter(observer_id == top_hotspot_id) %>%
  mutate(dist_m = haversine_m(lon, lat, start_lon, start_lat))

# ============================================================================
# MOVEMENT DATA PREP
# ============================================================================

# Find campus and hotspot origin points (first checklist in visit) to calculate

campus_origin_row <- campus_obs %>%
  filter(visit_id == top_campus_visit, checklist_order == 1) %>% slice(1)
hotspot_origin_row <- hotspot_obs %>%
  filter(visit_id == top_hotspot_visit, checklist_order == 1) %>% slice(1)

# Campus origin 

campus_origin_lon <- campus_origin_row$lon
campus_origin_lat <- campus_origin_row$lat
campus_origin_loc <- campus_origin_row$locality_id

# Hotspot origin

hotspot_origin_lon <- hotspot_origin_row$lon
hotspot_origin_lat <- hotspot_origin_row$lat
hotspot_origin_loc <- hotspot_origin_row$locality_id

# Filter to campus points within 50 km of origin and arrange by checklist order for mapping

campus_data <- campus_obs %>%
  filter(visit_id == top_campus_visit) %>%
  mutate(dist_from_origin = haversine_m(lon, lat,
                                        campus_origin_lon, campus_origin_lat)) %>%
  filter(dist_from_origin <= 50000) %>%
  arrange(checklist_order)


# Filter to hotspot points within 50 km of origin and arrange by checklist order for mapping

hotspot_data <- hotspot_obs %>%
  filter(visit_id == top_hotspot_visit) %>%
  mutate(dist_from_origin = haversine_m(lon, lat,
                                        hotspot_origin_lon, hotspot_origin_lat)) %>%
  filter(dist_from_origin <= 50000) %>%
  arrange(checklist_order)

# Summarize campus visits by unique locality 

campus_visits <- campus_data %>%
  group_by(lon, lat, locality_id) %>%
  summarize(n_visits = n(), .groups = "drop") %>%
  mutate(point_type = ifelse(locality_id == campus_origin_loc,
                             "Start", "Visited"))

# Summarize hotspot visits by unique locality 

hotspot_visits <- hotspot_data %>%
  group_by(lon, lat, locality_id) %>%
  summarize(n_visits = n(), .groups = "drop") %>%
  mutate(point_type = ifelse(locality_id == hotspot_origin_loc,
                             "Start", "Visited"))

# ============================================================================
# COUNTY SETUP AND TILT
# ============================================================================

# Campus/hotspot points to sf 

campus_pts_sf  <- st_as_sf(campus_visits,  coords = c("lon","lat"), crs = 4326)
hotspot_pts_sf <- st_as_sf(hotspot_visits, coords = c("lon","lat"), crs = 4326)

# Counties for mapping

campus_counties <- texas_counties %>%
  filter(NAME %in% (st_join(campus_pts_sf, texas_counties) %>%
                      st_drop_geometry() %>% distinct(NAME) %>%
                      pull(NAME) %>% na.omit()))
hotspot_counties <- texas_counties %>%
  filter(NAME %in% (st_join(hotspot_pts_sf, texas_counties) %>%
                      st_drop_geometry() %>% distinct(NAME) %>%
                      pull(NAME) %>% na.omit()))

# Tilt county base map

campus_counties_t  <- tilt_map(campus_counties,
                               y_tilt = y_tilt, angle_rotate = angle_rotate,
                               x_stretch = x_stretch, y_stretch = y_stretch)
hotspot_counties_t <- tilt_map(hotspot_counties,
                               y_tilt = y_tilt, angle_rotate = angle_rotate,
                               x_stretch = x_stretch, y_stretch = y_stretch)

# Convert tilted counties to data frames for geom_polygon

campus_counties_df  <- sf_to_df(campus_counties_t)
hotspot_counties_df <- sf_to_df(hotspot_counties_t)

# Tilt visit points and radius rings

campus_visits_t  <- tilt_coords(campus_visits)
hotspot_visits_t <- tilt_coords(hotspot_visits)

# Radius rings for mapping — centered on origin, sized to median distance

campus_ring_t  <- make_radius_ring_t(campus_origin_lon,  campus_origin_lat,
                                     campus_median_km)
hotspot_ring_t <- make_radius_ring_t(hotspot_origin_lon, hotspot_origin_lat,
                                     hotspot_median_km)

# ============================================================================
# BUILD SPIKES FOR MAPPING
# ============================================================================

global_max_v <- max(campus_visits_t$n_visits, hotspot_visits_t$n_visits)

campus_spikes  <- build_spikes_t(campus_visits_t,  campus_counties_df,
                                 max_v = global_max_v)
hotspot_spikes <- build_spikes_t(hotspot_visits_t, hotspot_counties_df,
                                 max_v = global_max_v)

# ============================================================================
# PANEL A — VIOLIN PLOT
# ============================================================================

p_violin <- ggplot(violin_data,
                   aes(x = location_type, y = dist_km,
                       fill = location_type)) +
  
  geom_violin(trim = TRUE, alpha = 0.85, linewidth = 1) +
  
  stat_summary(fun = median, geom = "crossbar",
               width = 0.3, linewidth = 0.7, color = "white") +
  
  scale_fill_manual(values = c("campus"  = campus_color,
                               "hotspot" = hotspot_color),
                    guide = "none") +
  
  scale_x_discrete(labels = c("campus"  = "Campus\nObservers",
                              "hotspot" = "Hotspot\nObservers")) +
  
  scale_y_continuous(limits = c(0, 50),
                     breaks = seq(0, 50, 10),
                     expand = c(0, 0)) +
  
  labs(x = NULL, y = "Distance from origin (km)") +
  
  theme_minimal() +
  theme(
    text               = element_text(size = 18, family = "sans"),
    axis.text.x        = element_text(size = 20, face = "bold"),
    axis.text.y        = element_text(size = 16, face = "bold"),
    axis.title.y       = element_text(size = 20, face = "bold",
                                      margin = margin(r = 15)),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85", size = 0.6),
    panel.grid.minor   = element_blank(),
    axis.line          = element_line(color = "black", size = 1),
    legend.position    = "none",
    plot.background    = element_rect(fill = "white", color = NA),
    plot.margin        = margin(20, 25, 20, 20)
  )

# ============================================================================
# PANEL B — TILTED RADII SCHEMATIC
# ============================================================================

radii_origin_lon <- campus_origin_lon
radii_origin_lat <- campus_origin_lat

campus_ring_t_radii  <- make_radius_ring_t(radii_origin_lon, radii_origin_lat,
                                           campus_median_km)
hotspot_ring_t_radii <- make_radius_ring_t(radii_origin_lon, radii_origin_lat,
                                           hotspot_median_km)

origin_t <- tilt_coords(
  data.frame(lon = radii_origin_lon, lat = radii_origin_lat), "lon", "lat"
)

radii_spike_h <- diff(range(campus_ring_t_radii$y_t)) * origin_frac

campus_lbl <- tilt_coords(data.frame(
  lon = radii_origin_lon - campus_median_km / (111 * cos(radii_origin_lat * pi/180)),
  lat = radii_origin_lat
), "lon", "lat")

hotspot_lbl <- tilt_coords(data.frame(
  lon = radii_origin_lon - hotspot_median_km / (111 * cos(radii_origin_lat * pi/180)),
  lat = radii_origin_lat
), "lon", "lat")

radii_all_x <- c(campus_ring_t_radii$x_t, hotspot_ring_t_radii$x_t)
radii_all_y <- c(campus_ring_t_radii$y_t, hotspot_ring_t_radii$y_t,
                 origin_t$y_t + radii_spike_h)
radii_xlim  <- c(min(radii_all_x) - diff(range(radii_all_x)) * 0.08,
                 max(radii_all_x) + diff(range(radii_all_x)) * 0.08)
radii_ylim  <- c(min(radii_all_y) - diff(range(radii_all_y)) * 0.08,
                 max(radii_all_y) + diff(range(radii_all_y)) * 0.12)

p_radii <- ggplot() +
  
  geom_path(data = hotspot_ring_t_radii, aes(x = x_t, y = y_t),
            color = hotspot_color, linewidth = 1.1, linetype = "dashed") +
  
  geom_path(data = campus_ring_t_radii, aes(x = x_t, y = y_t),
            color = campus_color, linewidth = 1.1, linetype = "dashed") +
  
  annotate("text",
           x = hotspot_lbl$x_t - diff(range(radii_all_x)) * 0.04,
           y = hotspot_lbl$y_t,
           label = "27.9 km", color = hotspot_color,
           fontface = "bold", size = 3.5, hjust = 1) +
  
  annotate("text",
           x = campus_lbl$x_t - diff(range(radii_all_x)) * 0.04,
           y = campus_lbl$y_t,
           label = "9.3 km", color = campus_color,
           fontface = "bold", size = 3.5, hjust = 1) +
  
  geom_segment(data = origin_t,
               aes(x = x_t, y = y_t, xend = x_t, yend = y_t + radii_spike_h),
               color = origin_color, linewidth = 1.2) +
  
  geom_point(data = origin_t,
             aes(x = x_t, y = y_t + radii_spike_h),
             shape = 21, fill = origin_color, color = "white",
             size = 6, stroke = 0.6) +
  
  geom_point(data = origin_t, aes(x = x_t, y = y_t),
             size = 1.5, color = "gray30") +
  
  coord_fixed(ratio = 1, xlim = radii_xlim, ylim = radii_ylim,
              expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(10, 10, 10, 10))

# ============================================================================
# PANELS C AND D — MOVEMENT MAPS
# ============================================================================

make_panel <- function(counties_df, spikes, ring_t,
                       line_color, size_max = 1, show_legend = FALSE) {
  
  all_poly_x <- counties_df$X
  all_poly_y <- counties_df$Y
  map_w <- diff(range(all_poly_x))
  map_h <- diff(range(all_poly_y))
  
  xlim <- c(min(all_poly_x) - map_w * 0.15,
            max(all_poly_x) + map_w * 0.15)
  ylim <- c(min(all_poly_y) - map_h * 0.08,
            max(spikes$y_top) + map_h * 0.40)
  
  sb <- make_scalebar(counties_df, map_w, map_h)
  
  ggplot() +
    
    # Uniform gray counties
    
    geom_polygon(data = counties_df,
                 aes(x = X, y = Y, group = group),
                 fill = "gray88", color = NA) +
    
    # Median radius ring
    
    geom_path(data = ring_t, aes(x = x_t, y = y_t),
              color = line_color, linewidth = 0.9,
              linetype = "dashed", inherit.aes = FALSE) +
    
    # Spike stems
    
    geom_segment(data = spikes %>% filter(point_type == "Visited"),
                 aes(x = x_base, y = y_base, xend = x_top, yend = y_top),
                 color = line_color, linewidth = 1.0, alpha = 0.9) +
    geom_segment(data = spikes %>% filter(point_type == "Start"),
                 aes(x = x_base, y = y_base, xend = x_top, yend = y_top),
                 color = origin_color, linewidth = 1.2, alpha = 0.9) +
    
    # Visited location caps — sized by visits
    
    geom_point(data = spikes %>% filter(point_type == "Visited"),
               aes(x = x_top, y = y_top, size = n_visits),
               shape = 21, fill = line_color,
               color = "white", stroke = 0.5, alpha = 0.95) +
    
    # Origin cap — fixed size
    
    geom_point(data = spikes %>% filter(point_type == "Start"),
               aes(x = x_top, y = y_top),
               shape = 21, fill = origin_color,
               color = "white", size = 6, stroke = 0.6, alpha = 0.95) +
    
    scale_size_continuous(
      range  = c(4, 12),
      limits = c(1, size_max),
      breaks = pretty(c(1, size_max), n = 4),
      name   = "Visits",
      guide  = if (show_legend)
        guide_legend(override.aes = list(shape = 21, fill = "gray50",
                                         color = "white"))
      else "none"
    ) +
    
    # Scale bar
    
    geom_rect(data = sb$blocks,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = I(sb$blocks$fill), color = "black",
              linewidth = 0.4, inherit.aes = FALSE) +
    geom_text(data = sb$labels,
              aes(x = x, y = y, label = label),
              size = 3.0, vjust = 1, inherit.aes = FALSE) +
    
    coord_fixed(ratio = 1, xlim = xlim, ylim = ylim,
                expand = FALSE, clip = "off") +
    theme_void() +
    theme(
      legend.position = if (show_legend) "right" else "none",
      legend.title    = element_text(size = 9),
      legend.text     = element_text(size = 8),
      plot.margin     = margin(10, 10, 10, 10),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

p_campus <- make_panel(
  campus_counties_df, campus_spikes, campus_ring_t,
  campus_color, size_max = global_max_v, show_legend = FALSE
)

p_hotspot <- make_panel(
  hotspot_counties_df, hotspot_spikes, hotspot_ring_t,
  hotspot_color, size_max = global_max_v, show_legend = TRUE
)

# ============================================================================
# SAVE PANELS INDIVIDUALLY
# ============================================================================

ggsave(file.path(output_dir, "Figure_5A_violin.png"),
       plot = p_violin,  width = 5,  height = 6, dpi = 300, bg = "white")
ggsave(file.path(output_dir, "Figure_5B_radii.png"),
       plot = p_radii,   width = 6,  height = 6, dpi = 300, bg = "white")
ggsave(file.path(output_dir, "Figure_5C_campus.png"),
       plot = p_campus,  width = 8,  height = 7, dpi = 300, bg = "white")
ggsave(file.path(output_dir, "Figure_5D_hotspot.png"),
       plot = p_hotspot, width = 8,  height = 7, dpi = 300, bg = "white")

# ============================================================================
# SAVE FIGURE DATA
# ============================================================================

write.csv(
  bind_rows(
    campus_data  %>% select(lat, lon, checklist_order) %>%
      mutate(observer_type = "Campus"),
    hotspot_data %>% select(lat, lon, checklist_order) %>%
      mutate(observer_type = "Hotspot")
  ),
  file.path(output_dir, "Figure_5_movement_data.csv"),
  row.names = FALSE
)
