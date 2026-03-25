##############################
#
# Figure S4: Sensitivity Analysis State Maps
# Ian Becker
# March 2026
#
##############################

# This script makes maps of campuses in states used for our geographic 
# sensitivity analysis. This makes Figure S4 in the supplementary materials. 

library(ggplot2)
library(sf)
library(tigris)
library(dplyr)
library(ggspatial)

options(tigris_use_cache = TRUE)

setwd("PATH HERE")

# ============================================================================
# LOAD MASTER DATA
# ============================================================================

# Load sensitivity analysis campus data

campus_sensitivity <- read.csv("sensitivity_test.csv")

# Define sensitivity states

sensitivity_states <- c("OR", "NJ", "AL")

# All contiguous US states for insets (load once)

us_all <- states(cb = TRUE) %>%
  filter(!STUSPS %in% c("AK", "HI", "PR", "VI", "GU", "MP", "AS")) %>%
  st_transform(crs = 4326)

# ============================================================================
# PLOTTING LOOP THROUGH STATES
# ============================================================================

for(state_abbr in sensitivity_states) {
  
  cat("\n========================================\n")
  cat("Processing state:", state_abbr, "\n")
  cat("========================================\n")
  
  # Filter campuses for this state
  
  state_campuses <- campus_sensitivity %>%
    filter(state_abbr == !!state_abbr)
  
  cat("Found", nrow(state_campuses), "campuses in", state_abbr, "\n")
  
  # Get state boundary
  
  state_boundary <- states(cb = TRUE) %>%
    filter(STUSPS == state_abbr) %>%
    st_transform(crs = 4326)
  
  # Convert campuses to sf
  
  campus_sf <- st_as_sf(state_campuses, 
                        coords = c("longitude", "latitude"), 
                        crs = 4326) %>%
    mutate(log_checklists = log(checklist_count + 1))
  
  # ============================================================
  # MAIN MAP
  # ============================================================
  
  main_map <- ggplot() +
    geom_sf(data = state_boundary, fill = "gray95", color = "gray40", linewidth = 0.5) +
    geom_sf(data = campus_sf, aes(size = log_checklists), 
            color = "#FFB703", alpha = 0.7) +
    scale_size_continuous(
      name = "Checklists (ln scale)",
      breaks = log(c(1, 10, 100, 1000) + 1),
      labels = c("1", "10", "100", "1000"),
      range = c(2, 7)
    ) +
    annotation_scale(
      location = "br",
      width_hint = 0.25,
      style = "ticks",
      text_cex = 1.5,
      pad_x = unit(0.5, "cm"),
      pad_y = unit(1.0, "cm")
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      plot.margin = unit(c(0.5, 0.5, 2.5, 0.5), "cm")
    )
  
  # Save main map
  
  main_filename <- paste0("figureSX_", state_abbr, "_campuses.png")
  ggsave(main_filename, plot = main_map, 
         width = 8, height = 10, dpi = 600, bg = "white")
  cat("✓ Saved:", main_filename, "\n")
  
  # ============================================================
  # INSET MAP
  # ============================================================
  
  inset_map <- ggplot() +
    geom_sf(data = us_all %>% mutate(highlight = STUSPS == state_abbr), 
            aes(fill = highlight), color = "gray40", linewidth = 0.3) +
    scale_fill_manual(values = c("FALSE" = "gray90", "TRUE" = "#FFB703")) +
    theme_void() +
    theme(legend.position = "none")
  
  # Save inset map
  
  inset_filename <- paste0("figureSX_", state_abbr, "_inset.png")
  ggsave(inset_filename, plot = inset_map,
         width = 4, height = 2.5, dpi = 600, bg = "white")
  cat("✓ Saved:", inset_filename, "\n")
  
}