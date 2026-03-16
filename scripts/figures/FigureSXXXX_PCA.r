##############################
#
# Figure SXXXX: PCA results figure
# Ian Becker
# March 2026
#
##############################

library(ggplot2)

campus_pca <- read.csv("campus_data_with_pca.csv")

ggplot(campus_pca, aes(x = IEI_PC1, y = IEI_PC2, color = factor(carnegie_ordinal))) +
  geom_point(size = 2.5, alpha = 0.7) +
  scale_color_manual(values = c("1" = "gray60", "2" = "steelblue", 
                                "3" = "orange", "4" = "darkred"),
                     labels = c("No Designation", "Research Colleges", "R2", "R1")) +
  labs(x = "PC1 (Institutional Capacity)", 
       y = "PC2 (Academic Programming)",
       color = "Carnegie Classification") +
  theme_minimal()
