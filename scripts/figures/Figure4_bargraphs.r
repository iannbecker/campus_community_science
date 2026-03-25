##############################
#
# Figure 4 Bar Graphs
# Ian Becker
# February 2026
#
##############################

# This script creates bar graphs comparing visitor dynamics between campuses
# and hotspots. This makes figure 4 in the main text

library(tidyverse)
library(ggplot2)
library(cowplot)

setwd("PATH HERE")
output_dir <- "PATH HERE"

# ============================================================================
# CALCULATE SUMMARY STATISTICS FOR PLOTTING 
# ============================================================================

# Read in data 

unique_visitors <- read.csv("unique_visitors_checklist_counts.csv")

# Summary statistics for unique visitors

visitor_summary_stats <- unique_visitors %>%
  group_by(location_type) %>%
  summarise(
    mean_visitors = mean(unique_visitors, na.rm = TRUE),
    sd_visitors = sd(unique_visitors, na.rm = TRUE),
    se_visitors = sd_visitors / sqrt(n()),
    n = n()
  )

# Summary statistics for checklists

checklist_summary_stats <- unique_visitors %>%
  group_by(location_type) %>%
  summarise(
    mean_checklists = mean(checklist_count, na.rm = TRUE),
    sd_checklists = sd(checklist_count, na.rm = TRUE),
    se_checklists = sd_checklists / sqrt(n()),
    n = n()
  )

# Combined summary for both metrics

combined_summary <- unique_visitors %>%
  group_by(location_type) %>%
  summarise(
    mean_visitors = mean(unique_visitors, na.rm = TRUE),
    se_visitors = sd(unique_visitors, na.rm = TRUE) / sqrt(n()),
    mean_checklists = mean(checklist_count, na.rm = TRUE),
    se_checklists = sd(checklist_count, na.rm = TRUE) / sqrt(n())
  )

# ============================================================================
# CREATE BAR PLOTS
# ============================================================================

# Bar plot for unique visitor

p1 <- ggplot(visitor_summary_stats, aes(x = location_type, y = mean_visitors, fill = location_type)) +
  geom_bar(stat = "identity", width = 0.7,
           color = "black", linewidth = 1) +
  geom_errorbar(aes(ymin = mean_visitors - se_visitors, 
                    ymax = mean_visitors + se_visitors),
                width = 0.2, size = 1.3, color = "black") +
  scale_fill_manual(values = c("campus" = "#FFB703", "hotspot" = "#023047")) +
  scale_x_discrete(labels = c("campus" = "Campus", "hotspot" = "Hotspot")) +
  labs(
    x = "",
    y = "# of Unique Visitors"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18, family = "sans"),
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 15)),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85", size = 0.6),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size = 1),
    legend.position = "none",
    plot.margin = margin(20, 25, 20, 20)
  )

# Bar plot for checklists

p2 <- ggplot(checklist_summary_stats, aes(x = location_type, y = mean_checklists, fill = location_type)) +
  geom_bar(stat = "identity", width = 0.7,
           color = "black", linewidth = 1) +
  geom_errorbar(aes(ymin = mean_checklists - se_checklists, 
                    ymax = mean_checklists + se_checklists),
                width = 0.2, size = 1.3, color = "black") +
  scale_fill_manual(values = c("campus" = "#FFB703", "hotspot" = "#023047")) +
  scale_x_discrete(labels = c("campus" = "Campus", "hotspot" = "Hotspot")) +
  labs(
    x = "",
    y = "# of Checklists"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18, family = "sans"),
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 15)),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85", size = 0.6),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size = 1),
    legend.position = "none",
    plot.margin = margin(20, 25, 20, 20)
  )

# Check plots

print(p1)
print(p2)

# Save plots

ggsave(path = output_dir, "Figure4_unique_visitors_comparison.png", p1, width = 8, height = 6, dpi = 300)
ggsave(path = output_dir, "Figure4_checklist_count_comparison.png", p2, width = 8, height = 6, dpi = 300)

# Get legend only

p_with_legend <- ggplot(visitor_summary_stats, aes(x = location_type, y = mean_visitors, fill = location_type)) +
  geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 1) +
  scale_fill_manual(
    values = c("campus" = "#FFB703", "hotspot" = "#023047"),
    labels = c("campus" = "Campus", "hotspot" = "Hotspot"),
    name = "Location Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16, face = "bold"),
    legend.key.size = unit(1.2, "cm"),
    legend.key = element_rect(color = "black", linewidth = 1.5, fill = NA),
    legend.spacing.x = unit(0.5, "cm")
  )

# Extract just the legend

legend <- get_legend(p_with_legend)

# Save the standalone legend

ggsave(path = output_dir, "legend_only_figure_4.png", legend, width = 6, height = 2, dpi = 300, bg = "white")

# Save summary table for figure

write.csv(combined_summary, "figure_4_combined_summary_stats.csv", row.names = FALSE)
