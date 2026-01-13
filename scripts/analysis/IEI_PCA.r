##############################
#
# Preliminary PCA: Institutional Engagement Index
# Using currently available variables
#
##############################

### This is currently an exploratory script to try PCA to make the IEI and run some 
### preliminary models 

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(lme4)
library(MASS)
library(glmmTMB)
library(performance)
library(DHARMa)

# ============================================================================
# LOAD DATA
# ============================================================================

campus <- read.csv("campus_data_with_counts_NO_ZERO.csv")

# ============================================================================
# PREPARE VARIABLES FOR PCA
# ============================================================================

campus_pca <- campus %>%
  mutate(
    # Highest degree (ordinal 1-4)
    degree_ordinal = case_when(
      str_detect(offering_highest_degree, "Associate") ~ 1,
      str_detect(offering_highest_degree, "Bachelor") ~ 2,
      str_detect(offering_highest_degree, "Master") ~ 3,
      str_detect(offering_highest_degree, "Doctor") ~ 4,
      TRUE ~ NA_real_
    ),
    # Land grant (binary)
    land_grant_binary = ifelse(land_grant == "Yes", 1, 0),
    # Offers graduate programs (binary)
    grad_program = ifelse(offering_grad == "Yes", 1, 0)
  )

# Select PCA input variables
pca_input <- campus_pca %>%
  dplyr::select(enrollment_Total, degree_ordinal, land_grant_binary) %>%
  drop_na()

cat("N institutions:", nrow(pca_input), "\n")
cat("Variables:", names(pca_input), "\n\n")

# ============================================================================
# CORRELATION CHECK
# ============================================================================

cat("--- Correlation Matrix ---\n")
print(round(cor(pca_input), 3))

# ============================================================================
# RUN PCA
# ============================================================================

pca_result <- PCA(pca_input, scale.unit = TRUE, graph = FALSE)

# ============================================================================
# RESULTS
# ============================================================================

# Variance explained
cat("\n--- Variance Explained ---\n")
print(get_eigenvalue(pca_result))

# PC1 loadings
cat("\n--- PC1 Loadings ---\n")
loadings <- pca_result$var$coord[, 1]
print(sort(abs(loadings), decreasing = TRUE))
print(loadings)

# Scree plot
fviz_eig(pca_result, addlabels = TRUE, main = "Preliminary IEI: Scree Plot")

# Variable contributions
fviz_contrib(pca_result, choice = "var", axes = 1, 
             title = "Variable Contributions to PC1")

# ============================================================================
# TEST MODEL
# ============================================================================

# Add PC1 scores to your data
campus_pca$IEI_PC1 <- NA
campus_pca$IEI_PC1[complete.cases(campus_pca[, c("enrollment_Total", "degree_ordinal", "land_grant_binary")])] <- 
  pca_result$ind$coord[, 1]

# Negative binomial GLMM with state as random effect

m1 <- glmmTMB(checklist_count ~ IEI_PC1 + (1 | state_abbr), 
              data = campus_pca, 
              family = nbinom2)

summary(m1)
r2(m1)

# Simulate residuals (gold standard for GLMMs)
sim_res <- simulateResiduals(m1, n = 1000)

# Visual check - should see uniform distribution, no patterns
plot(sim_res)

# Formal tests
testDispersion(sim_res)       # Overdispersion
testZeroInflation(sim_res)    # Excess zeros
testOutliers(sim_res)         # Influential points

# Residuals vs predictor (check linearity)
plotResiduals(sim_res, campus_pca$IEI_PC1)

