##############################
#
# Preliminary PCA: Institutional Engagement Index
# Using currently available variables
#
##############################

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(glmmTMB)
library(DHARMa)
library(performance)

# ============================================================================
# LOAD DATA
# ============================================================================

campus_pca <- read.csv("campus_filter_with_environment_SENSITIVITY_ANALYSIS.csv")

cat("Total rows:", nrow(campus_pca), "\n")
cat("Missing values: None - ready for PCA!\n")

# ============================================================================
# PCA WITH CAMPUS AREA INCLUDED (9 variables)
# ============================================================================

pca_vars <- c("enrollment_Total", "degree_ordinal", "land_grant_binary", 
              "carnegie_ordinal", "ecology.wildlife.count", "active.research.labs", 
              "bio.eco.major.offered", "student.birding.nature.club", "campus_area_km2")

# Select variables for PCA
pca_input <- campus_pca %>%
  dplyr::select(all_of(pca_vars))

cat("\nN for PCA:", nrow(pca_input), "\n")
cat("Variables:", ncol(pca_input), "\n")

# ============================================================================
# CORRELATION CHECK
# ============================================================================

cat("\n--- Correlation Matrix ---\n")
print(round(cor(pca_input), 2))

cat("\n--- Campus Area Correlations ---\n")
print(round(cor(pca_input)[,"campus_area_km2"], 3))

# ============================================================================
# RUN PCA
# ============================================================================

pca_result <- PCA(pca_input, scale.unit = TRUE, graph = FALSE)

# Variance explained
cat("\n--- Variance Explained ---\n")
print(get_eigenvalue(pca_result))

# PC1 loadings
cat("\n--- PC1 Loadings (with campus area) ---\n")
print(round(sort(pca_result$var$coord[,2], decreasing = TRUE), 3))

# Visualizations
fviz_eig(pca_result, addlabels = TRUE, 
         main = "IEI Scree Plot (9 variables with area)")

fviz_contrib(pca_result, choice = "var", axes = 1, 
             title = "Contributions to PC1 (with area)")

# ============================================================================
# ADD PC SCORES TO DATA
# ============================================================================

campus_pca$IEI_PC1 <- pca_result$ind$coord[, 1]
campus_pca$IEI_PC2 <- pca_result$ind$coord[, 2]
campus_pca$IEI_PC3 <- pca_result$ind$coord[, 3]

# Validation
cat("\n--- Top 5 by IEI (with area in PCA) ---\n")
print(campus_pca %>% 
        select(inst_name, IEI_PC1, campus_area_km2, checklist_count) %>% 
        arrange(desc(IEI_PC1)) %>% 
        head(5))

cat("\n--- Bottom 5 by IEI ---\n")
print(campus_pca %>% 
        select(inst_name, IEI_PC1, campus_area_km2, checklist_count) %>% 
        arrange(IEI_PC1) %>% 
        head(5))

# ============================================================================
# MODEL COMPARISON
# ============================================================================

# PC1 only (area now inside IEI)
m1 <- glmmTMB(checklist_count ~ IEI_PC1 + (1|state_abbr), 
              data = campus_pca, 
              family = nbinom2)

# PC1 + PC2
m2 <- glmmTMB(checklist_count ~ IEI_PC1 + IEI_PC2 + (1|state_abbr), 
              data = campus_pca, 
              family = nbinom2)

# PC1 + PC2 + PC3
m3 <- glmmTMB(checklist_count ~ IEI_PC1 + IEI_PC2 + IEI_PC3 + (1|state_abbr), 
              data = campus_pca, 
              family = nbinom2)

# Compare
cat("\n--- Model Comparison (with area in IEI) ---\n")
print(AIC(m1, m2, m3))

# Best model summary
cat("\n--- Best Model Summary ---\n")
summary(m2)

# Diagnostics
sim_res <- simulateResiduals(m2, n = 1000)
plot(sim_res)

# Model fit
cat("\n--- Model Fit ---\n")
cat("Pearson R²:", cor(campus_pca$checklist_count, fitted(m2))^2, "\n")
print(r2(m2))

# ============================================================================
# TEST OTHER CANDIDATE MODELS 
# ============================================================================

# Standardize everything

campus_pca <- campus_pca %>%
  mutate(
    # Standardize environmental variables (mean = 0, SD = 1)
    impervious_5km_scaled = scale(impervious_5km)[,1],
    impervious_10km_scaled = scale(impervious_10km)[,1],
    pct_vegetation_scaled = scale(pct_vegetation_campus)[,1],
    latitude_scaled = scale(latitude)[,1],
    longitude_scaled = scale(longitude)[,1],
    
    # PCs are already standardized from PCA, but you can re-standardize if you want consistency
    # (Not strictly necessary since PCA already did this)
    # PC1_scaled = scale(IEI_PC1)[,1],
    # PC2_scaled = scale(IEI_PC2)[,1]
  )

# Check means and SDs (should be ~0 and ~1)

cat("\nStandardized variable summaries:\n")
summary(campus_pca[, c("impervious_5km_scaled", "impervious_10km_scaled", "pct_vegetation_scaled")])

# Candidate models

m_null <- glmmTMB(checklist_count ~ 1 + (1|state_abbr), 
              data = campus_pca, 
              family = nbinom2)

m_inst <- glmmTMB(checklist_count ~ IEI_PC1 + IEI_PC2 + (1|state_abbr), 
              data = campus_pca, 
              family = nbinom2)

m_urban <- glmmTMB(checklist_count ~ IEI_PC1 + IEI_PC2 + impervious_10km + (1|state_abbr), 
                   data = campus_pca, 
                   family = nbinom2)

m_vege <- glmmTMB(checklist_count ~ IEI_PC1 + IEI_PC2 + pct_vegetation_scaled + (1|state_abbr), 
                         data = campus_pca, 
                         family = nbinom2)

# model selection

aic_results <- AIC(m_null, m_inst, m_urban, m_vege)
print(aic_results)

# check best model

summary(m_urban)
sim_res_urban <- simulateResiduals(m_urban, n = 1000)
plot(sim_res_urban)
cat("Pearson R²:", cor(campus_pca$checklist_count, fitted(m_urban))^2, "\n")
print(r2(m_urban))
