##################
#
#  Figure SX: Observer distribution
#  Ian Becker
#  February 2026
#
##################

library(ggplot2)
library(dplyr)
library(patchwork)

# Load campus data

campus_obs <- read.csv("campus_bird_observations_raw.csv")
single_UTRGV <- read.csv("single_campus_The_University_of_Texas_Rio_Grande_Valley___Brownsville_Campus.csv")

# Change to match column name

campus_obs$location_id <- as.character(campus_obs$location_id)
single_UTRGV$location_id <- as.character(single_UTRGV$location_id)

# Combine Campus and Hotspot data

campus_obs <- bind_rows(campus_obs, single_UTRGV)


# Calculate per-campus metrics

campus_summary <- campus_obs %>%
  group_by(location_id) %>%
  summarize(
    n_observers = n_distinct(observer_id),
    n_checklists = n_distinct(checklist_id),
    .groups = "drop"
  )

# Summary stats for caption

cat("Observer distribution:\n")
cat("Median:", median(campus_summary$n_observers), "\n")
cat("Range:", min(campus_summary$n_observers), "-", max(campus_summary$n_observers), "\n\n")

cat("Checklist distribution:\n")
cat("Median:", median(campus_summary$n_checklists), "\n")
cat("Range:", min(campus_summary$n_checklists), "-", max(campus_summary$n_checklists), "\n\n")


# Linear scale histograms (raw data)

p1_linear <- ggplot(campus_summary, aes(x = n_observers)) +
  geom_histogram(bins = 30, fill = "#FFB703", color = "black", alpha = 0.8) +
  labs(x = "Observers per campus", y = "Count", title = "(A) Linear scale") +
  theme_minimal()

p2_linear <- ggplot(campus_summary, aes(x = n_checklists)) +
  geom_histogram(bins = 30, fill = "#FFB703", color = "black", alpha = 0.8) +
  labs(x = "Checklists per campus", y = "Count", title = "(B) Linear scale") +
  theme_minimal()

# Natural log scale 

p1_log <- ggplot(campus_summary, aes(x = n_observers)) +
  geom_histogram(bins = 30, fill = "#FFB703", color = "black", alpha = 0.8) +
  scale_x_continuous(trans = "log", breaks = c(1, 10, 100, 1000)) +
  labs(x = "Observers per campus (ln scale)", y = "Count", title = "(C) Log scale") +
  theme_minimal()

p2_log <- ggplot(campus_summary, aes(x = n_checklists)) +
  geom_histogram(bins = 30, fill = "#FFB703", color = "black", alpha = 0.8) +
  scale_x_continuous(trans = "log", breaks = c(1, 10, 100, 1000, 10000)) +
  labs(x = "Checklists per campus (ln scale)", y = "Count", title = "(D) Log scale") +
  theme_minimal()

# Combine: linear on top, log on bottom

combined <- (p1_linear + p2_linear) / (p1_log + p2_log)

ggsave("FigureSX_sampling_distribution_both.png", combined, width = 10, height = 8, dpi = 600)
