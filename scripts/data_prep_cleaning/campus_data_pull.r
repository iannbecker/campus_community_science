##################
#
#  Campus Data Pull
#  Ian Becker
#  Dec 2025
#
##################

# This script is used for the initial campus dataset pull from the IPEDS database

library(educationdata)
library(dplyr)

#################### Data pull

# Pull IPEDS directory data for 2023 - most recent year

ipeds_raw <- get_education_data(
  level = "college-university",
  source = "ipeds",
  topic = "directory",
  filters = list(year = 2023,
                 fips = c(48, 40, 20)),  # TX=48, OK=40, KS=20
  add_labels = TRUE
)

################### Data filter

# Filter out non-degree granting and trade schools

campus_data <- ipeds_raw %>%
  filter(offering_highest_level %in% c("Associate's degree",
                                       "Bachelor's degree", 
                                       "Master's degree",
                                       "Post-master's certificate",
                                       "Doctor's degree"))

# Filter to only columns of interest