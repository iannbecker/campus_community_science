##################
#
#  OSM Polygon Pull
#  Ian Becker
#  Dec 2025
#
##################

# This script is used to pull campus polygons from OSM using filtered IPEDS data

library(osmdata)
library(sf)
library(dplyr)

# Load filtered IPEDS data

campus_data <- read.csv("data/campus_data_pull_manual_filter.csv")

# Function to query OSM for university polygons near a point

get_campus_polygon <- function(lat, lon, name, search_radius = 1000) {
  
  # Create bounding box around point (in meters)
  bbox <- st_point(c(lon, lat)) %>%
    st_sfc(crs = 4326) %>%
    st_transform(3857) %>%  # Project to meters
    st_buffer(search_radius) %>%
    st_transform(4326) %>%
    st_bbox()
  
  # Query OSM for university/college amenities
  query <- opq(bbox = bbox) %>%
    add_osm_feature(key = "amenity", value = c("university", "college")) %>%
    osmdata_sf()
  
  # Check if polygons exist
  if (!is.null(query$osm_polygons) && nrow(query$osm_polygons) > 0) {
    return(list(found = TRUE, geometry = query$osm_polygons))
  } else {
    return(list(found = FALSE, geometry = NULL))
  }
}

# Initialize results

campus_data$polygon_found <- NA
campus_polygons <- list()

# Loop through each campus

for(i in 1:nrow(campus_data)) {
  
  cat("Processing:", campus_data$inst_name[i], "\n")
  
  result <- tryCatch({
    get_campus_polygon(
      lat = campus_data$latitude[i],
      lon = campus_data$longitude[i],
      name = campus_data$inst_name[i]
    )
  }, error = function(e) {
    list(found = FALSE, geometry = NULL)
  })
  
  campus_data$polygon_found[i] <- result$found
  
  if(result$found) {
    campus_polygons[[i]] <- result$geometry
  }
  
  Sys.sleep(1)  # to handle OSM servers
}

# Save results

write.csv(campus_data, "data/campus_with_polygon_status.csv", row.names = FALSE)

# Save campuses needing manual polygons

no_polygon <- campus_data %>% filter(!polygon_found)
write.csv(no_polygon, "data/processed/campuses_need_manual_polygons.csv", row.names = FALSE)

