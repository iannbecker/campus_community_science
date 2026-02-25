##############################
#
# Campus eBird metrics analysis
# Ian Becker
# February 2026
#
##############################

library(tidyverse)
library(ggplot2)
library(car)

setwd("~/Desktop/project_code/campus_community_science/data")

# ============================================================================
# LOAD AND PREP DATA
# ============================================================================

# Unique Visitors data

unique_visitors <- read.csv("unique_visitors_comparison.csv")

# Visit summary data

visit_summary <- read.csv("visitor_summary_by_type.csv")

# Separate campus and hotspot data

campus_visitors <- unique_visitors %>%
  filter(location_type == "campus")

hotspot_visitors <- unique_visitors %>%
  filter(location_type == "hotspot")

# ============================================================================
# VISITOR ANALYSIS
# ============================================================================

# check normality assumption of unique visitors

shapiro.test(campus_visitors$unique_visitors)

shapiro.test(hotspot_visitors$unique_visitors)

# Levene test for homogeneity of variance

leveneTest(unique_visitors ~ location_type, data = unique_visitors)

# Non-parametric test: Wilcoxon rank-sum test (Mann Whitney U Test)

wilcox.test(unique_visitors ~ location_type, data = unique_visitors)

# ============================================================================
# CHECKLIST ANALYSIS
# ============================================================================

# Check normality assumption of total checklists

shapiro.test(campus_visitors$checklist_count)

shapiro.test(hotspot_visitors$checklist_count)

# Levene test for homogeneity of variance

leveneTest(checklist_count ~ location_type, data = unique_visitors)

# Non-parametric test: Wilcoxon rank-sum test (Mann Whitney U Test)

wilcox.test(checklist_count ~ location_type, data = unique_visitors)

# ============================================================================
# CHECKLISTS PER OBSERVER (AXING THIS)
# ============================================================================

# Calculate checklists per unique visitor

concentration <- unique_visitors %>%
  mutate(checklists_per_observer = checklist_count / unique_visitors)

# Separate campus and hotspot data

campus_concentration <- concentration %>%
  filter(location_type == "campus")

hotspot_concentration <- concentration %>%
  filter(location_type == "hotspot")

# Check normality assumption of checklists per observer

shapiro.test(campus_concentration$checklists_per_observer)

shapiro.test(hotspot_concentration$checklists_per_observer)

# Levene test for homogeneity of variance

leveneTest(checklists_per_observer ~ location_type, data = concentration)

# Non-parametric test: Wilcoxon rank-sum test (Mann Whitney U Test)

wilcox.test(checklists_per_observer ~ location_type, data = concentration)

# ============================================================================
# SELECT CAMPUS AND HOTSPOT FOR MOVEMENT FIGURE
# ============================================================================

# Select high-activity campus

top_campus <- campus_visitors %>%
  arrange(desc(checklist_count)) %>%
  slice(1)

# Select comparable hotspot (similar checklist volume)

comparable_hotspot <- hotspot_visitors %>%
  arrange(desc(checklist_count)) %>%
  slice(1:10) %>%  # Look at top 10 hotspots
  slice(1)  # Pick one with similar activity

# Show candidates

hotspot_visitors %>%
  arrange(desc(checklist_count)) %>%
  select(location_name, checklist_count, unique_visitors) %>%
  head(10)

# check maximum visitors 
max(campus_visitors$unique_visitors)
max(hotspot_visitors$unique_visitors)
