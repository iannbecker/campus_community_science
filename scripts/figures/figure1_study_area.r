##############################
#
# Figure 1: Study Area Map
# Ian Becker
# January 2026
#
##############################

library(ggplot2)
library(sf)
library(terra)
library(tigris)
library(dplyr)
library(ggspatial)

options(tigris_use_cache = TRUE)
setwd("~/Desktop/project_code/campus_community_science/data")

# ============================================================================
# LOAD AND PREP DATA
# ============================================================================

# Load in campus data

campus <- read.csv("campus_data_with_environment.csv")


# Load state boundaries (TX, KS, OK)

states <- states(cb = TRUE) %>%
  filter(STUSPS %in% c("TX", "KS", "OK")) %>%
  st_transform(crs = 5070)  # Albers Equal Area

# Load spatial points from campus data

campus_sf <- st_as_sf(campus, coords = c("longitude", "latitude"), 
                      crs = 4326) %>%
  st_transform(crs = 5070)

# Match campus CRS to states

campus_sf <- st_transform(campus_sf, st_crs(states))

# ============================================================================
# PLOT MAP
# ============================================================================

# Create the map

study_area_map <- ggplot() +
  # County boundaries
  geom_sf(data = states, fill = "gray95", color = "gray40", linewidth = 0.5) +
  
  # Study sites as points - COLOR NOW IN AES()
  geom_sf(data = campus_sf, aes(color = "College Campus (n = 153)"), size = 2, alpha = 0.7) +
  
  # Define the color and legend label
  scale_color_manual(name = "", values = c("College Campus (n = 153)" = "darkgreen")) +
  
  # Scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    style = "ticks",
    text_cex = 0.8
  ) +
  
  # North arrow
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +
  
  # Clean theme
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 8),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray50"),
    legend.position = "bottom"  # or "right", "top", "left"
  ) +
  labs(
    x = "Longitude", y = "Latitude"
  )

print(study_area_map)

# Save
ggsave("figure1_study_area.png", plot = study_area_map, 
       width = 10, height = 8, dpi = 300, bg = "white")



