##############################
#
# Community Analysis
# Ian Becker
# March 2026
#
##############################

# This script calculates species richness, similarity, and rare species occurrence 
# between campuses and hotspots. It also makes the venn diagram used for figure 6 in the main text.

library(tidyverse)
library(sf)
library(vegan)

setwd("PATH HERE")

# ============================================================================
# COMBINE OBSERVATION DATA (PRELIMINARY)
# ============================================================================

# Load in Campus and Hotspot data

campus_obs <- read.csv("campus_bird_observations_raw.csv")
hotspot_obs <- read.csv("all_bird_observations_raw.csv")
single_UTRGV <- read.csv("single_campus_The_University_of_Texas_Rio_Grande_Valley___Brownsville_Campus.csv")

# Transform location id to character in both datasets

campus_obs$location_id <- as.character(campus_obs$location_id)
hotspot_obs$location_id <- as.character(hotspot_obs$location_id)
single_UTRGV$location_id <- as.character(single_UTRGV$location_id)
  
# Combine Campus and Hotspot data

all_obs <- bind_rows(campus_obs, hotspot_obs, single_UTRGV)

# Check all observations 

print(all_obs %>% 
        group_by(location_type) %>% 
        summarize(
          n_obs = n(),
          n_checklists = n_distinct(checklist_id),
          n_species = n_distinct(scientific_name),
          n_locations = n_distinct(location_id)
        ))

# Save combined data

write.csv(all_obs, "combined_bird_observations.csv", row.names = FALSE)

# Clean up

rm(campus_obs, hotspot_obs, single_UTRGV)

# ============================================================================
# RICHNESS PER LOCATION
# ============================================================================

# Load in data (if not already done)

all_obs <- read.csv("combined_bird_observations.csv")

richness_by_location <- all_obs %>%
  group_by(location_id, location_type, state) %>%
  summarize(
    species_richness = n_distinct(scientific_name),
    n_checklists = n_distinct(checklist_id),
    n_observations = n(),
    n_observers = n_distinct(observer_id),
    .groups = "drop"
  )

# Summary stats

summary_stats <- richness_by_location %>%
  group_by(location_type) %>%
  summarize(
    n_locations = n(),
    mean_richness = mean(species_richness),
    median_richness = median(species_richness),
    sd_richness = sd(species_richness),
    min_richness = min(species_richness),
    max_richness = max(species_richness),
    .groups = "drop"
  )

print(summary_stats)

# Mann-Whitney U test

campus_rich <- richness_by_location %>% filter(location_type == "campus")
hotspot_rich <- richness_by_location %>% filter(location_type == "hotspot")

wilcox_test <- wilcox.test(campus_rich$species_richness, 
                           hotspot_rich$species_richness)

# Save richness data

write.csv(richness_by_location, "species_richness_by_location.csv", row.names = FALSE)
write.csv(summary_stats, "richness_summary_stats.csv", row.names = FALSE)

# ============================================================================
# COMMUNITY ANALYSIS
# ============================================================================

# Total species by type

pooled_species <- all_obs %>%
  group_by(location_type) %>%
  summarize(
    total_species = n_distinct(scientific_name),
    total_checklists = n_distinct(checklist_id),
    total_observations = n()
  )

print(pooled_species)

# Get species lists

campus_species <- all_obs %>% 
  filter(location_type == "campus") %>% 
  pull(scientific_name) %>% 
  unique()

hotspot_species <- all_obs %>% 
  filter(location_type == "hotspot") %>% 
  pull(scientific_name) %>% 
  unique()

shared_species <- intersect(campus_species, hotspot_species)
total_unique <- union(campus_species, hotspot_species)

# Calculate indices

jaccard_index <- length(shared_species) / length(total_unique)
sorensen_index <- (2 * length(shared_species)) / (length(campus_species) + length(hotspot_species))

# ============================================================================
# RARE SPECIES BY OCCURRENCE
# ============================================================================

# Calculate presence/absence at each location

species_by_location <- all_obs %>%
  group_by(scientific_name, common_name, location_type, location_id) %>%
  summarize(present = 1, .groups = "drop")

# Count locations for each species

species_occupancy <- species_by_location %>%
  group_by(scientific_name, common_name, location_type) %>%
  summarize(
    n_locations = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = location_type, 
              values_from = n_locations,
              values_fill = 0) %>%
  mutate(
    campus_occupancy = (campus / 153) * 100,  # 153 campuses
    hotspot_occupancy = (hotspot / 153) * 100  # 153 hotspots
  )



# Use 2.5% threshold (found at ≤2.5% of locations)

campus_rare <- species_occupancy %>%
  filter(campus_occupancy > 0, campus_occupancy <= 2.5) %>%
  pull(scientific_name)

hotspot_rare <- species_occupancy %>%
  filter(hotspot_occupancy > 0, hotspot_occupancy <= 2.5) %>%
  pull(scientific_name)

all_rare <- union(campus_rare, hotspot_rare)
shared_rare <- intersect(campus_rare, hotspot_rare)

# Get campus-only and hotspot-only rare species names

campus_only_rare <- species_occupancy %>%
  filter(campus_occupancy > 0, campus_occupancy <= 2.5,
         hotspot_occupancy == 0) %>%
  select(scientific_name, common_name, campus_occupancy) %>%
  arrange(campus_occupancy)

hotspot_only_rare <- species_occupancy %>%
  filter(hotspot_occupancy > 0, hotspot_occupancy <= 2.5,
         campus_occupancy == 0) %>%
  select(scientific_name, common_name, hotspot_occupancy) %>%
  arrange(hotspot_occupancy)

# Get most common shared species (highest occurrence in both)

shared_common <- species_occupancy %>%
  filter(campus_occupancy > 0, hotspot_occupancy > 0) %>%
  mutate(avg_occupancy = (campus_occupancy + hotspot_occupancy) / 2) %>%
  arrange(desc(avg_occupancy)) %>%
  select(scientific_name, common_name, campus_occupancy, hotspot_occupancy, avg_occupancy)

# ============================================================================
# VENN DIAGRAM
# ============================================================================

library(eulerr)

# Create proportional Venn diagram

venn_data <- euler(c(
  "Campus" = 60,      # campus only rare
  "Hotspot" = 66,     # hotspot only rare
  "Campus&Hotspot" = 441  # shared species
))

plot(venn_data,
     fills = list(fill = c("#FFB703", "#023047"), alpha = 1),
     quantities = FALSE,
     labels = FALSE)

