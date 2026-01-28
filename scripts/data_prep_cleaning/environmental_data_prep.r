##############################
#
# Environmental Data Prep
# Ian Becker
# January 2026
#
##############################

# This script is used to prep land cover and urbanization data for modelling

library(sf)
library(terra)
library(dplyr)

setwd("~/Desktop/project_code/campus_community_science/data")

# ============================================================================
# LOAD IN DATA 
# ============================================================================

# Load in urbanization data

urban_nlcd <- rast("NLCD_impervious.tif")

# Load in land cover data

land_cover_nlcd <- rast("NLCD_landcover.tif")

# Load in campus boundaries 

campus_boundaries <- st_read("campus_polygons_complete")

# Load in campus data 

campus_data <- read.csv("campus_data_with_IEI.csv")

# ============================================================================
# PREP DATA FOR ENVIRONMENTAL VARIABLES
# ============================================================================

# Fix UTRGV - Edinburg name in shapefile

campus_boundaries <- campus_boundaries %>%
  mutate(inst_name = ifelse(inst_name == "The University of Texas Rio Grande Valley", 
                            "The University of Texas Rio Grande Valley - Edinburg", 
                            inst_name))

# Filter campus boundaries to only those with IEI data

campus_boundaries_filtered <- campus_boundaries %>%
  filter(inst_name %in% campus_data$inst_name)

# ============================================================================
# EXTRACT URBANIZATION DATA
# ============================================================================

# Create centroids from polygons

campus_centroids <- st_centroid(campus_boundaries_filtered)

# Transform to projected CRS for buffering (meters)

campus_centroids_proj <- st_transform(campus_centroids, 5070)  # Albers Equal Area

# Create 5km and 10km buffers

campus_buffers_5km <- st_buffer(campus_centroids_proj, dist = 5000)
campus_buffers_10km <- st_buffer(campus_centroids_proj, dist = 10000)

# Transform buffers to match raster CRS

campus_buffers_5km <- st_transform(campus_buffers_5km, crs(urban_nlcd))
campus_buffers_10km <- st_transform(campus_buffers_10km, crs(urban_nlcd))

# Extract mean impervious surface within buffers

impervious_5km <- terra::extract(
  urban_nlcd, 
  vect(campus_buffers_5km),
  fun = mean,
  na.rm = TRUE,
  ID = TRUE
)

impervious_10km <- terra::extract(
  urban_nlcd, 
  vect(campus_buffers_10km),
  fun = mean,
  na.rm = TRUE,
  ID = TRUE
)

# Add to campus data

campus_boundaries_filtered$impervious_5km <- impervious_5km$NLCD_impervious
campus_boundaries_filtered$impervious_10km <- impervious_10km$NLCD_impervious

# Compare distributions

par(mfrow = c(1, 2))
hist(campus_boundaries_filtered$impervious_5km, 
     main = "5km Buffer Impervious %",
     xlab = "Mean Impervious %")
hist(campus_boundaries_filtered$impervious_10km,
     main = "10km Buffer Impervious %", 
     xlab = "Mean Impervious %")

# Check correlation between buffer sizes

cat("\nCorrelation between 5km and 10km buffers:", 
    cor(campus_boundaries_filtered$impervious_5km, 
        campus_boundaries_filtered$impervious_10km), "\n")

# ============================================================================
# EXTRACT ON-CAMPUS GREEN SPACE
# ============================================================================

# Use the campus polygons directly (not buffers) for habitat quality

campus_boundaries_proj <- st_transform(campus_boundaries_filtered, crs(land_cover_nlcd))

# Extract land cover within campus boundaries

landcover_values <- terra::extract(
  land_cover_nlcd,
  vect(campus_boundaries_proj),
  ID = TRUE
)

# Calculate % vegetation per campus

vegetation_summary <- landcover_values %>%
  group_by(ID) %>%
  summarize(
    total_pixels = n(),
    veg_pixels = sum(NLCD_landcover %in% c(41, 42, 43, 52, 71, 90, 95), na.rm = TRUE),
    pct_vegetation = (veg_pixels / total_pixels) * 100
  )

campus_boundaries_filtered$pct_vegetation_campus <- vegetation_summary$pct_vegetation

# Check distribution

hist(campus_boundaries_filtered$pct_vegetation_campus,
     main = "On-Campus Vegetation Cover %",
     xlab = "Vegetation %")

# Merge both urban for now

campus_data_env <- campus_data %>%
  left_join(
    campus_boundaries_filtered %>%
      st_drop_geometry() %>%
      select(inst_name, impervious_5km, impervious_10km, pct_vegetation_campus),
    by = "inst_name"
  )

# Quick correlation check

cat("\nCorrelation with checklist counts:\n")
cat("5km buffer:", cor(campus_data_env$checklist_count, 
                       campus_data_env$impervious_5km, use = "complete.obs"), "\n")
cat("10km buffer:", cor(campus_data_env$checklist_count, 
                        campus_data_env$impervious_10km, use = "complete.obs"), "\n")

# Save

write.csv(campus_data_env, "campus_data_with_environment.csv", row.names = FALSE)
