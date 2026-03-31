##############################
#
# Institutional Engagement: PCA and Models
# Ian Becker
# Mar 2026
# 
##############################

# This script is used to prep PCA variables, run PCA,
# and subsequently run models representing competing hypotheses

library(tidyverse)
library(readxl)
library(FactoMineR)
library(factoextra)
library(glmmTMB)
library(DHARMa)
library(performance)
library(MuMIn)

setwd("~/Desktop/project_code/campus_community_science/data")

# ============================================================================
# LOAD AND MERGE DATA
# ============================================================================

campus_ <- read.csv("campus_data_with_counts.csv")
carnegie <- read_excel("carnegie_classification.xlsx", sheet = "data")

# Merge datasets

campus <- manual %>%
  left_join(campus %>% dplyr::select(unitid, state_abbr, enrollment_Total, 
                                          offering_highest_degree, land_grant, 
                                          longitude, latitude), 
            by = "unitid") %>%
  left_join(carnegie %>% dplyr::select(unitid, research2025name), 
            by = "unitid")

# ============================================================================
# CREATE PCA VARIABLES
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
    
    # Research labs (binary)
    
    research_labs = ifelse(`active research labs` == "y", 1, 0),
    
    # Bio major (binary)
    
    bio_major = ifelse(`bio/eco major offered` == "y", 1, 0),
    
    # Birding club (binary)
    
    birding_club = ifelse(`student birding/nature club` == "y", 1, 0),
    
    # Carnegie research (ordinal)
    
    carnegie_ordinal = case_when(
      research2025name == "No research designation" ~ 1,
      research2025name == "Research Colleges and Universities" ~ 2,
      research2025name == "Research 2: High Spending and Doctorate Production" ~ 3,
      research2025name == "Research 1: Very High Spending and Doctorate Production" ~ 4,
      TRUE ~ NA_real_
    ),
   
    # Faculty count (already numeric)
    
    faculty_count = `ecology/wildlife faculty`
  )

# ============================================================================
# PCA (9 variables)
# ============================================================================

# Denote variables for PCA

pca_vars <- c("enrollment_Total", "degree_ordinal", "land_grant_binary", 
              "carnegie_ordinal", "faculty_count", "research_labs", 
              "bio_major", "birding_club", "campus_area_km2")

pca_input <- campus_pca %>%
  dplyr::select(all_of(pca_vars)) %>%
  drop_na()

# Check correlation

print(round(cor(pca_input), 2))

# Run PCA

pca_result <- PCA(pca_input, scale.unit = TRUE, graph = FALSE)

# Variance explained

print(get_eigenvalue(pca_result))

# PC1 loadings

print(round(sort(pca_result$var$coord[,1], decreasing = TRUE), 3))

# PC2 loadings

print(round(sort(pca_result$var$coord[,2], decreasing = TRUE), 3))

# Plot PCA results

fviz_eig(pca_result, addlabels = TRUE, main = "IEI Scree Plot (9 variables)")
fviz_contrib(pca_result, choice = "var", axes = 1, title = "Contributions to PC1")

# ============================================================================
# ADD PC SCORES TO DATA
# ============================================================================

# Only add scores to rows with complete PCA data

complete_rows <- complete.cases(campus_pca[, pca_vars])
campus_pca$IEI_PC1 <- NA
campus_pca$IEI_PC2 <- NA
campus_pca$IEI_PC3 <- NA
campus_pca$IEI_PC1[complete_rows] <- pca_result$ind$coord[, 1]
campus_pca$IEI_PC2[complete_rows] <- pca_result$ind$coord[, 2]
campus_pca$IEI_PC3[complete_rows] <- pca_result$ind$coord[, 3]

# Check PCA scores

# Top 5 by PC1

print(campus_pca %>% 
        dplyr::select(inst_name, IEI_PC1, checklist_count) %>% 
        arrange(desc(IEI_PC1)) %>% 
        head(5))

# Bottom 5 by PC1

print(campus_pca %>% 
        dplyr::select(inst_name, IEI_PC1, checklist_count) %>% 
        arrange(IEI_PC1) %>% 
        head(5))

campus_pca <- campus_pca %>%
  mutate(
    impervious_5km_scaled = scale(impervious_5km)[,1],
    impervious_10km_scaled = scale(impervious_10km)[,1],
    pct_vegetation_scaled = scale(pct_vegetation_campus)[,1]
  )

# ============================================================================
# MODELLING
# ============================================================================

# Candidate models

m_null <- glmmTMB(checklist_count ~ 1 + (1|state_abbr), 
                  data = campus_pca, 
                  family = nbinom2)

m_inst <- glmmTMB(checklist_count ~ IEI_PC1 + IEI_PC2 + (1|state_abbr), 
                  data = campus_pca, 
                  family = nbinom2)

m_urban <- glmmTMB(checklist_count ~ IEI_PC1 + IEI_PC2 + impervious_10km_scaled + (1|state_abbr), 
                   data = campus_pca, 
                   family = nbinom2)

m_vege <- glmmTMB(checklist_count ~ IEI_PC1 + IEI_PC2 + pct_vegetation_scaled + (1|state_abbr), 
                  data = campus_pca, 
                  family = nbinom2)

# Model Selection by AIC

aic_results <- AIC(m_null, m_inst, m_urban, m_vege)

aic_results$delta_AIC <- aic_results$AIC - min(aic_results$AIC)

aic_results$AIC_weight <- Weights(aic_results$AIC)

print(aic_results)

# ============================================================================
# BEST MODEL CHECKS
# ============================================================================

summary(m_urban)

# Check residuals

sim_res <- simulateResiduals(m_urban, n = 1000)
plot(sim_res)

# Check dispersion 

testDispersion(sim_res)

# Check r-squared

print(r2(m_urban))

# ============================================================================
# SAVE 
# ============================================================================

write.csv(campus_pca, "campus_data_with_pca.csv", row.names = FALSE)
saveRDS(m_urban, "best_model_urban.rds")
