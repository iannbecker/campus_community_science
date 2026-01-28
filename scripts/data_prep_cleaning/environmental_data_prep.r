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

