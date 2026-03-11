##############################
#
# Bird Analysis Campus vs. Hotspots
# Using currently available variables
#
##############################

library(tidyverse)
library(sf)
library(vegan)

setwd("~/Desktop/project_code/campus_community_science/data")

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

cat("===========================================\n")
cat("SPECIES RICHNESS BY LOCATION\n")
cat("===========================================\n")

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

cat("\n--- Mann-Whitney U Test ---\n")
cat("Campus median:", median(campus_rich$species_richness), "\n")
cat("Hotspot median:", median(hotspot_rich$species_richness), "\n")
cat("W =", wilcox_test$statistic, "\n")
cat("p-value =", format(wilcox_test$p.value, digits = 3), "\n\n")

# Save richness data
write.csv(richness_by_location, "species_richness_by_location.csv", row.names = FALSE)
write.csv(summary_stats, "richness_summary_stats.csv", row.names = FALSE)
cat("✓ Saved richness files\n\n")

##############################
# 2. POOLED COMMUNITY ANALYSIS
##############################

cat("===========================================\n")
cat("POOLED COMMUNITY SIMILARITY\n")
cat("===========================================\n")

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

cat("\n--- Community Similarity ---\n")
cat("Campus species:", length(campus_species), "\n")
cat("Hotspot species:", length(hotspot_species), "\n")
cat("Shared species:", length(shared_species), "\n")
cat("Total unique species:", length(total_unique), "\n")
cat("Jaccard index:", round(jaccard_index, 3), 
    "(", round(jaccard_index * 100, 1), "% overlap)\n")
cat("Sorensen index:", round(sorensen_index, 3), "\n\n")

##############################
# RARE SPECIES - BY LOCATION OCCUPANCY
##############################

cat("===========================================\n")
cat("RARE SPECIES BY LOCATION OCCUPANCY\n")
cat("===========================================\n")

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

# Test different thresholds
cat("=== OCCUPANCY DISTRIBUTION ===\n")
for (threshold in c(25, 20, 15, 10, 5, 2.5, 1)) {
  n_rare_campus <- sum(species_occupancy$campus_occupancy > 0 & species_occupancy$campus_occupancy <= threshold)
  n_rare_hotspot <- sum(species_occupancy$hotspot_occupancy > 0 & species_occupancy$hotspot_occupancy <= threshold)
  cat("At ≤", threshold, "% of locations:\n")
  cat("  Campus rare:", n_rare_campus, "\n")
  cat("  Hotspot rare:", n_rare_hotspot, "\n\n")
}

# Use 10% threshold (found at ≤10% of locations)
cat("=== USING 2.5% THRESHOLD ===\n")

campus_rare <- species_occupancy %>%
  filter(campus_occupancy > 0, campus_occupancy <= 2.5) %>%
  pull(scientific_name)

hotspot_rare <- species_occupancy %>%
  filter(hotspot_occupancy > 0, hotspot_occupancy <= 2.5) %>%
  pull(scientific_name)

all_rare <- union(campus_rare, hotspot_rare)
shared_rare <- intersect(campus_rare, hotspot_rare)

cat("Rare species (≤2.5% location occupancy):\n")
cat("Total rare species:", length(all_rare), "\n")
cat("Rare at campuses:", length(campus_rare), "\n")
cat("Rare at hotspots:", length(hotspot_rare), "\n")
cat("Rare at both:", length(shared_rare), "\n")
cat("Campus-only rare:", length(setdiff(campus_rare, hotspot_rare)), "\n")
cat("Hotspot-only rare:", length(setdiff(hotspot_rare, campus_rare)), "\n\n")

##############################
# SUMMARY FOR RESULTS
##############################

cat("===========================================\n")
cat("SUMMARY FOR RESULTS SECTION\n")
cat("===========================================\n")

cat("\nSpecies Richness:\n")
cat("Campuses (median =", median(campus_rich$species_richness), 
    ", range =", min(campus_rich$species_richness), "-", max(campus_rich$species_richness), ")\n")
cat("Hotspots (median =", median(hotspot_rich$species_richness), 
    ", range =", min(hotspot_rich$species_richness), "-", max(hotspot_rich$species_richness), ")\n")
cat("Mann-Whitney: W =", wilcox_test$statistic, ", p =", 
    format(wilcox_test$p.value, digits = 3), "\n\n")

cat("Community Overlap:\n")
cat("Jaccard index =", round(jaccard_index, 3), 
    "(", round(jaccard_index * 100, 1), "% species overlap)\n\n")

cat("Rare Species:\n")
cat(rare_summary$total_rare, "species occur in ≤2.5% of checklists\n")
cat(rare_summary$campus_total, "(", 
    round(rare_summary$campus_total / rare_summary$total_rare * 100, 1), 
    "%) found on campuses\n")
cat(rare_summary$hotspot_total, "(", 
    round(rare_summary$hotspot_total / rare_summary$total_rare * 100, 1), 
    "%) found at hotspots\n")

cat("\n===========================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("===========================================\n")

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

print(campus_only_rare)
print(hotspot_only_rare, n = 25)

# Get most common shared species (highest occurrence in both)
shared_common <- species_occupancy %>%
  filter(campus_occupancy > 0, hotspot_occupancy > 0) %>%
  mutate(avg_occupancy = (campus_occupancy + hotspot_occupancy) / 2) %>%
  arrange(desc(avg_occupancy)) %>%
  select(scientific_name, common_name, campus_occupancy, hotspot_occupancy, avg_occupancy)

cat("Top 10 most common shared species:\n")
print(shared_common, n = 50)

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

