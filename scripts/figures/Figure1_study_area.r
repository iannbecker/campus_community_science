##############################
#
# Figure 1: Study Area Map
# Ian Becker
# January 2026
#
##############################

# This script creates figure 1 in the main text 

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

# Load in campus data

campus <- read.csv("campus_data_with_pca.csv")

# Load state boundaries (TX, KS, OK)

states <- states(cb = TRUE) %>%
  filter(STUSPS %in% c("TX", "KS", "OK")) %>%
  st_transform(crs = 4326)  

# Load spatial points from campus data

campus_sf <- st_as_sf(campus, coords = c("longitude", "latitude"), 
                      crs = 4326) %>%
  st_transform(crs = 4326)

# Add log-transformed checklist count for point sizing

campus_sf <- campus_sf %>%
  mutate(log_checklists = log(checklist_count + 1))  

# ============================================================================
# PLOT MAP
# ============================================================================

study_area_map <- ggplot() +
  
  # State boundaries
  
  geom_sf(data = states, fill = "gray95", color = "gray40", linewidth = 0.5) +
  
  # Campus points sized by log(checklists)
  
  geom_sf(data = campus_sf, aes(size = log_checklists), 
          color = "#FFB703", alpha = 0.7) +
  
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

ggsave("figure1_study_area.png", plot = study_area_map, 
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
  scale_fill_manual(values = c("FALSE" = "gray90", "TRUE" = "#FFB703")) +
  theme_void() +
  theme(legend.position = "none")

ggsave("figure1_inset_us.png", plot = inset_map,
       width = 4, height = 2.5, dpi = 600, bg = "white")
