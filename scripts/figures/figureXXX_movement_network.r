##############################
#
# Zoomed Movement Paths - SIMPLIFIED
# Points only, star for start
#
##############################

library(tidyverse)
library(sf)
library(maps)
library(ggplot2)
library(patchwork)

##############################
# LOAD DATA
##############################

tx_network <- read.csv("network_7day_tamu_blucher.csv")

tx_network <- tx_network %>%
  mutate(location_type = ifelse(origin == "Texas A&M", "Campus", "Hotspot"))

texas <- map_data("state") %>% filter(region == "texas")

# Create sequential paths
observer_paths <- tx_network %>%
  arrange(observer_id, checklist_order) %>%
  group_by(observer_id, origin, location_type) %>%
  mutate(
    next_lat = lead(lat),
    next_lon = lead(lon),
    has_next = !is.na(next_lat)
  ) %>%
  ungroup()

# Pick one exemplar from each
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
# CALCULATE ZOOM BOUNDS
##############################

# Campus zoom
campus_data <- exemplar_paths %>% filter(location_type == "Campus")
campus_lon_range <- range(c(campus_data$lon, campus_data$next_lon), na.rm = TRUE)
campus_lat_range <- range(c(campus_data$lat, campus_data$next_lat), na.rm = TRUE)

tight_buffer <- 0.5

campus_bounds <- list(
  xmin = campus_lon_range[1] - tight_buffer,
  xmax = campus_lon_range[2] + tight_buffer,
  ymin = campus_lat_range[1] - tight_buffer,
  ymax = campus_lat_range[2] + tight_buffer
)

# Hotspot zoom
hotspot_data <- exemplar_paths %>% filter(location_type == "Hotspot")
hotspot_lon_range <- range(c(hotspot_data$lon, hotspot_data$next_lon), na.rm = TRUE)
hotspot_lat_range <- range(c(hotspot_data$lat, hotspot_data$next_lat), na.rm = TRUE)

hotspot_bounds <- list(
  xmin = hotspot_lon_range[1] - tight_buffer,
  xmax = hotspot_lon_range[2] + tight_buffer,
  ymin = hotspot_lat_range[1] - tight_buffer,
  ymax = hotspot_lat_range[2] + tight_buffer
)

##############################
# CAMPUS MAP - SIMPLIFIED
##############################

p_campus <- ggplot() +
  geom_polygon(data = texas, 
               aes(x = long, y = lat, group = group),
               fill = "gray95", color = "gray50", linewidth = 0.3) +
  # Movement arrows
  geom_segment(data = campus_data %>% filter(has_next),
               aes(x = lon, y = lat,
                   xend = next_lon, yend = next_lat),
               color = "#FFB703", alpha = 0.8, linewidth = 1,
               arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  # All visited locations as small points
  geom_point(data = campus_data %>% filter(checklist_order != 1),
             aes(x = lon, y = lat),
             fill = "#FFB703", color = "black", size = 2.5, shape = 21, stroke = 0.5, alpha = 0.25) +
  # Starting point as STAR
  geom_point(data = campus_data %>% filter(checklist_order == 1),
             aes(x = lon, y = lat),
             color = "black", size = 1, shape = 8, stroke = 2) +  # shape 8 = star
  coord_quickmap(
    xlim = c(campus_bounds$xmin, campus_bounds$xmax),
    ylim = c(campus_bounds$ymin, campus_bounds$ymax)
  ) +
  theme_void() +
  theme(
    text = element_text(size = 18),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(title = "Campus")

##############################
# HOTSPOT MAP - SIMPLIFIED
##############################

p_hotspot <- ggplot() +
  geom_polygon(data = texas, 
               aes(x = long, y = lat, group = group),
               fill = "gray95", color = "gray50", linewidth = 0.3) +
  geom_segment(data = hotspot_data %>% filter(has_next),
               aes(x = lon, y = lat,
                   xend = next_lon, yend = next_lat),
               color = "#023047", alpha = 0.8, linewidth = 1,
               arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  geom_point(data = hotspot_data %>% filter(checklist_order != 1),
             aes(x = lon, y = lat),
             fill = "#023047", color = "black", size = 2.5, shape = 21, stroke = 0.5) +
  # Starting point as STAR
  geom_point(data = hotspot_data %>% filter(checklist_order == 1),
             aes(x = lon, y = lat),
             color = "black", size = 1, shape = 8, stroke = 2) +
  coord_quickmap(
    xlim = c(hotspot_bounds$xmin, hotspot_bounds$xmax),
    ylim = c(hotspot_bounds$ymin, hotspot_bounds$ymax)
  ) +
  theme_void() +
  theme(
    text = element_text(size = 18),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(title = "Hotspot")

##############################
# COMBINE
##############################

p_combined <- p_campus | p_hotspot

ggsave("texas_movement_simplified.png", p_combined, width = 14, height = 7, dpi = 300)

cat("\nSimplified map saved: texas_movement_simplified.png\n")
cat("- Small points for visited locations\n")
cat("- Star (✱) for starting point\n")
cat("- Arrows show movement sequence\n")