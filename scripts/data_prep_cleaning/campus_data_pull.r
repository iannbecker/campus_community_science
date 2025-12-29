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

campus_data <- campus_data %>%
  select(inst_name, state_abbr, county_name, offering_highest_level, 
         offering_highest_degree, tribal_college, urban_centric_locale, offering_undergrad, 
         offering_grad, inst_status, degree_granting, open_public, land_grant, inst_size,
         longitude, latitude)

# Filter out tribal schools, schools not offering undergrad, schools not offering degrees

campus_data <- campus_data %>%
  filter(tribal_college == "No",
         offering_undergrad == "Yes",
         degree_granting == "Yes")

# Remove columns

campus_data <- campus_data %>%
  select(-tribal_college, -offering_undergrad, -degree_granting)

# Remove any NA for institution size

campus_data <- campus_data %>%
  filter(!inst_size == "Not applicable")

# Save data for manual filtering 

write.csv(campus_data, "data/campus_data_pull_raw.csv", row.names = FALSE)
