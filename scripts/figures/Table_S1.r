##############################
#
# Table SXXX: Metrics Table 
# Ian Becker
# March 2026
#
##############################

library(tidyverse)

setwd("~/Desktop/project_code/campus_community_science/data")
campus_pca <- read.csv("campus_data_with_pca.csv")

# Range on all metrics

metrics_summary <- campus_pca %>%
  select(checklist_count, enrollment_Total, ecology.wildlife.faculty, 
         campus_area_km2, pct_vegetation_campus, impervious_10km,
         IEI_PC1, IEI_PC2) %>%
  summarise(across(everything(), 
                   list(min = min, 
                        max = max, 
                        median = median, 
                        mean = mean),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(), 
               names_to = c("variable", ".value"), 
               names_sep = "_(?=min|max|median|mean)")

print(metrics_summary)
write.csv(metrics_summary, "TableSX_metrics_ranges.csv", row.names = FALSE)






