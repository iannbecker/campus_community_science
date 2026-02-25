####################################
#
#  Campus Data Pull - Sensitivity Analysis
#  Ian Becker
#  Jan 2026
#
###################################

# This script is used for the initial campus dataset pull from the IPEDS database

library(educationdata)
library(dplyr)
library(tigris)

options(tigris_use_cache = TRUE)

# ============================================================================
# FLYWAY-BASED STATE SELECTION
# ============================================================================

# Define states by flyway (excluding Central Flyway states: MT, ND, SD, WY, NE, KS, CO, NM, OK, TX)
# and excluding Iowa (already included in your dataset)

flyway_states <- list(
  # Atlantic Flyway
  atlantic = tibble(
    state_abbr = c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "PA", 
                   "DE", "MD", "VA", "WV", "NC", "SC", "GA", "FL"),
    fips = c(23, 33, 50, 25, 44, 9, 36, 34, 42, 
             10, 24, 51, 54, 37, 45, 13, 12)
  ),
  
  # Mississippi Flyway (excluding Iowa and Central Flyway overlap)
  mississippi = tibble(
    state_abbr = c("MN", "WI", "IL", "IN", "OH", "MI", 
                   "KY", "TN", "AL", "MS", "LA"),
    fips = c(27, 55, 17, 18, 39, 26, 
             21, 47, 1, 28, 22)
  ),
  
  # Pacific Flyway
  pacific = tibble(
    state_abbr = c("AK", "WA", "OR", "CA", "ID", "NV", "UT", "AZ"),
    fips = c(2, 53, 41, 6, 16, 32, 49, 4)
  )
)

# Set seed for reproducibility

set.seed(23)

# Randomly select one state from each flyway

sampled_states <- bind_rows(
  flyway_states$atlantic %>% slice_sample(n = 1) %>% mutate(flyway = "Atlantic"),
  flyway_states$mississippi %>% slice_sample(n = 1) %>% mutate(flyway = "Mississippi"),
  flyway_states$pacific %>% slice_sample(n = 1) %>% mutate(flyway = "Pacific")
)

cat("=== FLYWAY SENSITIVITY ANALYSIS ===\n")
cat("Randomly selected one state from each flyway:\n")
print(sampled_states)

# Extract FIPS codes for the educationdata query

selected_fips <- sampled_states$fips

# ============================================================================
# DATA PULL
# ============================================================================

# Pull IPEDS directory data for 2024 - most recent year

ipeds_raw <- get_education_data(
  level = "college-university",
  source = "ipeds",
  topic = "directory",
  filters = list(year = 2023,
                 fips = selected_fips),
  add_labels = TRUE
)

cat("\nInitial campuses pulled:", nrow(ipeds_raw), "\n")

# ============================================================================
# DATA FILTER
# ============================================================================

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

cat("Campuses after filtering:", nrow(campus_data), "\n")
cat("\nCampuses by state:\n")
print(campus_data %>% count(state_abbr))

# ============================================================================
# ADD ENROLLMENT
# ============================================================================

# Pull IPEDS enrollment data for 2021 - most recent year

ipeds_raw <- get_education_data(
  level = "college-university",
  source = "ipeds",
  topic = "enrollment-headcount",
  filters = list(year = 2021,
                 fips = selected_fips),
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

# Add flyway information

campus_data <- campus_data %>%
  left_join(sampled_states %>% select(state_abbr, flyway, fips), by = "state_abbr")

# ============================================================================
# SAVE OUTPUT
# ============================================================================

# Create informative filename with states and flyways
output_filename <- paste0("data/campus_data_pull_raw_SENSITIVITY_FLYWAY_",
                          paste(sampled_states$state_abbr, collapse = "_"),
                          ".csv")

write.csv(campus_data, output_filename, row.names = FALSE)

# Save state selection for reproducibility

write.csv(sampled_states, 
          "data/sampled_flyway_states.csv",
          row.names = FALSE)

cat("\nData saved to:", output_filename, "\n")

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n=== SUMMARY ===\n")
cat("Random seed: 23\n")
cat("Atlantic Flyway:", sampled_states$state_abbr[sampled_states$flyway == "Atlantic"], "\n")
cat("Mississippi Flyway:", sampled_states$state_abbr[sampled_states$flyway == "Mississippi"], "\n")
cat("Pacific Flyway:", sampled_states$state_abbr[sampled_states$flyway == "Pacific"], "\n")
cat("Total campuses pulled:", nrow(campus_data), "\n\n")
cat("Campuses by state and flyway:\n")
print(campus_data %>% count(state_abbr, flyway))
