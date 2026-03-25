##############################
#
# Figure S3: Study Area Map (Hotspots)
# Ian Becker
# January 2026
#
##############################

# This script makes Figure S3 in the supplementary material. This makes a study 
# area map of all included hotspots in our study. 

library(ggplot2)
library(sf)
library(terra)
library(tigris)
library(dplyr)
library(ggspatial)

options(tigris_use_cache = TRUE)
setwd("PATH HERE")

# ============================================================================
# LOAD AND PREP DATA
# ============================================================================

# Load in data

data <- read.csv("all_bird_observations_raw.csv")

# Extract lat/lon for all unique hotspots

hotspot_locs <- data %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  select(locality, latitude, longitude) %>%
  distinct()

# Get rid of raw observations

rm(data)

# Load in visitor comparison data

visitors <- read.csv("unique_visitors_comparison.csv")

# Filter to just hotspots

hotspot <- visitors %>%
  rename(locality = location_name) %>%
  filter(location_type == "hotspot")

# Join to coordinates

hotspot <- hotspot %>%
  left_join(hotspot_locs, by = "locality") 

# Load state boundaries (TX, KS, OK)

states <- states(cb = TRUE) %>%
  filter(STUSPS %in% c("TX", "KS", "OK")) %>%
  st_transform(crs = 4326)  

# Load spatial points from campus data

hotspot_sf <- st_as_sf(hotspot, coords = c("longitude", "latitude"), 
                      crs = 4326) %>%
  st_transform(crs = 4326)

# Add log-transformed checklist count for point sizing

hotspot_sf <- hotspot_sf %>%
  mutate(log_checklists = log(checklist_count + 1))

# ============================================================================
# PLOT MAP
# ============================================================================

study_area_map <- ggplot() +
  
  # State boundaries
  
  geom_sf(data = states, fill = "gray95", color = "gray40", linewidth = 0.5) +
  
  # Campus points sized by log(checklists)
  
  geom_sf(data = hotspot_sf, aes(size = log_checklists), 
          color = "#023047", alpha = 0.7) +
  
  # Size scale
  
  scale_size_continuous(
    name = "Checklists (ln scale)",
    breaks = log(c(1, 10, 100, 1000) + 1),
    labels = c("1", "10", "100", "1000"),
    range = c(0.5, 4)
  ) +
  
  # Scale bar - bottom left
  
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    style = "ticks",
    text_cex = 0.8,
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  
  # North arrow - top left
  
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +
  
  # Clean theme - no axes
  
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

print(study_area_map)

# Save

ggsave("FigureS3_hotspot_map.png", plot = study_area_map, 
       width = 8, height = 8, dpi = 600, bg = "white")

# ============================================================================
# MINI STUDY AREA CONTEXT MAP
# ============================================================================

# All contiguous US states

us_all <- states(cb = TRUE) %>%
  filter(!STUSPS %in% c("AK", "HI", "PR", "VI", "GU", "MP", "AS")) %>%
  st_transform(crs = 4326) %>%
  mutate(highlight = STUSPS %in% c("TX", "KS", "OK"))

inset_map <- ggplot() +
  geom_sf(data = us_all, aes(fill = highlight), color = "gray40", linewidth = 0.3) +
  scale_fill_manual(values = c("FALSE" = "gray90", "TRUE" = "#023047")) +
  theme_void() +
  theme(legend.position = "none")

ggsave("FigureS3_inset_us.png", plot = inset_map,
       width = 4, height = 2.5, dpi = 600, bg = "white")
