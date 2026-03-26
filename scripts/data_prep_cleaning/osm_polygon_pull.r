##################
#
#  OSM Campus Polygon Pull
#  Ian Becker
#  Jan 2026
#
##################

# This script pulls campus polygons from OpenStreetMap using filtered IPEDS data
# Output: Shapefile with unitid and inst_name for joining checklist counts

library(osmdata)
library(sf)
library(dplyr)

setwd("")

# =============================================================================
# LOAD DATA
# =============================================================================

campus_data <- read.csv("data/campus_data.csv")

nrow(campus_data)

# =============================================================================
# FUNCTION: Query OSM for campus polygons (with retry logic + multipolygon support)
# =============================================================================

get_campus_polygon <- function(unitid, lat, lon, name, search_radius = 1000, max_retries = 3) {
  
  cat("Processing:", name, "\n")
  
  # Create bounding box around point (search_radius in meters)
  
  bbox <- st_point(c(lon, lat)) %>%
    st_sfc(crs = 4326) %>%
    st_transform(3857) %>%
    st_buffer(search_radius) %>%
    st_transform(4326) %>%
    st_bbox()
  
  # Retry loop
  
  for (attempt in 1:max_retries) {
    
    result <- tryCatch({
      
      # Query OSM with longer timeout
      
      query <- opq(bbox = bbox, timeout = 120) %>%
        add_osm_feature(key = "amenity", value = c("university", "college")) %>%
        osmdata_sf()
      
      # Check both polygons AND multipolygons
      
      polys <- query$osm_polygons
      mpolys <- query$osm_multipolygons
      
      # Combine if both exist
      
      if (!is.null(polys) && nrow(polys) > 0 && !is.null(mpolys) && nrow(mpolys) > 0) {
        all_polys <- rbind(
          st_sf(geometry = st_geometry(polys)),
          st_sf(geometry = st_geometry(mpolys))
        )
      } else if (!is.null(mpolys) && nrow(mpolys) > 0) {
        all_polys <- st_sf(geometry = st_geometry(mpolys))
      } else if (!is.null(polys) && nrow(polys) > 0) {
        all_polys <- st_sf(geometry = st_geometry(polys))
      } else {
        return(list(success = TRUE, data = NULL))  # No polygon found, but no error
      }
      
      # Get the largest polygon (most likely main campus)
      
      all_polys$area <- st_area(all_polys)
      largest_idx <- which.max(all_polys$area)
      polygon <- all_polys[largest_idx, ] %>% st_geometry()
      
      # Create sf object with campus metadata
      
      campus_sf <- st_sf(
        unitid = unitid,
        inst_name = name,
        latitude = lat,
        longitude = lon,
        osm_found = TRUE,
        geometry = polygon,
        crs = 4326
      )
      
      return(list(success = TRUE, data = campus_sf))
      
    }, error = function(e) {
      return(list(success = FALSE, error = e$message))
    })
    
    # If successful (even if no polygon found), return
    
    if (result$success) {
      if (!is.null(result$data)) {
        cat("  -> Found polygon\n")
      } else {
        cat("  -> No polygon found\n")
      }
      return(result$data)
    }
    
    # If error and retries left, wait and retry
    
    if (attempt < max_retries) {
      wait_time <- 2^attempt * 3  # 6, 12, 24 seconds
      cat("  Attempt", attempt, "failed:", result$error, "\n")
      cat("  Waiting", wait_time, "sec before retry...\n")
      Sys.sleep(wait_time)
    } else {
      cat("  FAILED after", max_retries, "attempts:", result$error, "\n")
    }
  }
  
  # All retries exhausted
  
  return(NULL)
}

# =============================================================================
# PROCESS ALL CAMPUSES
# =============================================================================

campus_polygons_list <- list()

for(i in 1:nrow(campus_data)) {
  
  cat(sprintf("[%d/%d] ", i, nrow(campus_data)))
  
  result <- get_campus_polygon(
    unitid = campus_data$unitid[i],
    lat = campus_data$latitude[i],
    lon = campus_data$longitude[i],
    name = campus_data$inst_name[i]
  )
  
  if (!is.null(result)) {
    campus_polygons_list[[length(campus_polygons_list) + 1]] <- result
  }
  
  # Account for OSM server timeout
  
  Sys.sleep(3)
}

# =============================================================================
# COMBINE AND SAVE RESULTS
# =============================================================================

if (length(campus_polygons_list) > 0) {
  
  # Combine all polygons
  
  campus_polygons_sf <- do.call(rbind, campus_polygons_list)
  
  # Print summary
  
  cat("\n===========================================\n")
  cat("RESULTS\n")
  cat("===========================================\n")
  cat("Total campuses attempted:", nrow(campus_data), "\n")
  cat("Polygons found:", nrow(campus_polygons_sf), "\n")
  cat("Polygons missing:", nrow(campus_data) - nrow(campus_polygons_sf), "\n")
  cat("Success rate:", round(nrow(campus_polygons_sf)/nrow(campus_data)*100, 1), "%\n")
  
  # Create output directory if needed
  
  if (!dir.exists("data/processed")) {
    dir.create("data/processed", recursive = TRUE)
  }
  
  # Save as Shapefile 
  
  st_write(campus_polygons_sf, 
           "data/processed/campus_polygons.shp", 
           delete_dsn = TRUE)
  cat("Saved to: data/processed/campus_polygons.shp\n")
  
  # Create summary file with polygon status for all campuses
  
  campus_summary <- campus_data %>%
    mutate(polygon_found = unitid %in% campus_polygons_sf$unitid)
  
  write.csv(campus_summary, 
            "data/processed/campus_with_polygon_status.csv", 
            row.names = FALSE)
  
  # Create list of campuses needing manual polygon creation
  
  no_polygon <- campus_summary %>% 
    filter(!polygon_found) %>%
    select(unitid, inst_name, latitude, longitude, state_abbr, urban_centric_locale)
  
  write.csv(no_polygon, 
            "data/processed/campuses_need_manual_polygons.csv", 
            row.names = FALSE)
  
  cat("\nCampuses without polygons:", nrow(no_polygon), "\n")
  cat("Saved to: data/processed/campuses_need_manual_polygons.csv\n")
  
} else {
  cat("\nERROR: No polygons found for any campus!\n")
}