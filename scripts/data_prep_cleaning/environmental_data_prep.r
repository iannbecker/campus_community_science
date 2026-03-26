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

setwd("PATH HERE")

# ============================================================================
# LOAD AND PREP DATA 
# ============================================================================

# Load in urbanization data

urban_nlcd <- rast("NLCD_impervious.tif")

# Load in land cover data

land_cover_nlcd <- rast("NLCD_landcover.tif")

# Load in campus boundaries 

campus_boundaries <- st_read("campus_polygons_MASTER")

# Load in campus data 

campus_data <- read.csv("campus_filtered_data.csv")

# Filter campus boundaries to only those with engagement data

campus_boundaries_filtered <- campus_boundaries %>%
  filter(inst_name %in% campus_data$inst_name)

# ============================================================================
# EXTRACT URBANIZATION DATA
# ============================================================================

# Create centroids from polygons

campus_centroids <- st_centroid(campus_boundaries_filtered)

# Transform to projected CRS for buffering (meters)

campus_centroids_proj <- st_transform(campus_centroids, 5070)  

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

# Check distributions

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

# Use the campus polygons for habitat quality

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

# Merge environmental data with master dataset

campus_data_env <- campus_data %>%
  left_join(
    campus_boundaries_filtered %>%
      st_drop_geometry() %>%
      select(inst_name, impervious_5km, impervious_10km, pct_vegetation_campus),
    by = "inst_name"
  )

# Save dataset

write.csv(campus_data_env, "campus_filter_with_environment.csv", row.names = FALSE)
