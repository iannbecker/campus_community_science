##############################
#
# Figure 3 (+ S6, S7) - Checklists vs. Predictors
# Ian Becker
# February 2026
#
##############################

# This script makes 3 figures (3, S6, S7) showing the relationship between
# checklist count and separate model predictors 

library(tidyverse)
library(ggplot2)
library(patchwork)

setwd("PATH HERE")
output_dir <- "PATH HERE"

# ============================================================================
# LOAD DATA AND MODEL
# ============================================================================

best_model <- readRDS("best_model_urban.rds")
campus_pca <- read.csv("campus_data_with_pca.csv")

# ============================================================================
# PC1 (INSTITUTIONAL SCALE) PLOT (Figure 3)
# ============================================================================

p_pc1 <- ggplot(campus_pca, aes(x = IEI_PC1, y = log(checklist_count + 1))) +
  geom_point(alpha = 0.6, size = 3, color = "gray40") +
  geom_smooth(method = "lm", se = TRUE, 
              color = "#FFB703", fill = "#FFB703", 
              alpha = 0.2, linewidth = 1.2) +
  labs(
    x = "Institutional Scale (PC1)",
    y = "Checklist Count (log)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18, family = "sans"),
    axis.text.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 15)),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85", linewidth = 0.6),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1),
    plot.margin = margin(20, 25, 20, 20),
  )

print(p_pc1)

ggsave(file.path(output_dir, "pc1_relationship.png"), 
       p_pc1, width = 8, height = 6, dpi = 300)

# ============================================================================
# PC2 (ACADEMIC PROGRAMMING) PLOT (Figure S6)
# ============================================================================

p_pc2 <- ggplot(campus_pca, aes(x = PC2, y = log(checklist_count + 1))) +
  geom_point(alpha = 0.6, size = 3, color = "gray40") +
  geom_smooth(method = "lm", se = TRUE, 
              color = "#FFB703", fill = "#FFB703", 
              alpha = 0.3, linewidth = 1.2) +
  labs(
    x = "Academic Programming (PC2)",
    y = "Checklist Count (log)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18, family = "sans"),
    axis.text.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 15)),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85", linewidth = 0.6),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1),
    plot.margin = margin(20, 25, 20, 20)
  )

print(p_pc2)

ggsave(file.path(output_dir, "pc2_relationship.png"), 
       p_impervious, width = 8, height = 6, dpi = 300)

# ============================================================================
# IMPERVIOUS PLOT Figure S7)
# ============================================================================

p_impervious <- ggplot(campus_pca, aes(x = impervious_10km, y = log(checklist_count + 1))) +
  geom_point(alpha = 0.6, size = 3, color = "gray40") +
  geom_smooth(method = "lm", se = TRUE, 
              color = "#FFB703", fill = "#FFB703", 
              alpha = 0.3, linewidth = 1.2) +
  labs(
    x = "% Impervious Surface (10km buffer)",
    y = "Checklist Count (log)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18, family = "sans"),
    axis.text.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 15)),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85", linewidth = 0.6),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1),
    plot.margin = margin(20, 25, 20, 20)
  )

print(p_impervious)

ggsave(file.path(output_dir, "impervious_relationship.png"), 
       p_impervious, width = 8, height = 6, dpi = 300)
