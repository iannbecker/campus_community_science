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
  select(unitid, inst_name, state_abbr, county_name, offering_highest_level, 
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

############### Add in enrollment numbers

# Pull IPEDS enrollment data for 2021 - most recent year

ipeds_raw <- get_education_data(
  level = "college-university",
  source = "ipeds",
  topic = "enrollment-headcount",
  filters = list(year = 2021,
                 fips = c(48, 40, 20)),  # TX=48, OK=40, KS=20
  add_labels = TRUE
)

# Only keep headcount rows where 'sex' = total

ipeds_campus <- ipeds_raw %>%
  filter(sex == "Total") 

# Get rid of race column and add up all headcounts to get total enrollment

ipeds_campus <- ipeds_campus %>%
  select(-race) %>%
  group_by(unitid, year, fips, level_of_study) %>%
  summarise(total_enrollment = sum(headcount, na.rm = TRUE)) %>%
  ungroup()

# Add undergrad, grad, and total enrollment columns

ipeds_campus_wide <- ipeds_campus %>%
  tidyr::pivot_wider(names_from = level_of_study,
                     values_from = total_enrollment,
                     names_prefix = "enrollment_")

# Join to campus data by unitid

campus_data <- campus_data %>%
  left_join(ipeds_campus_wide, by = "unitid")

# Save data for manual filtering 

write.csv(campus_data, "data/campus_data_pull_raw.csv", row.names = FALSE)

