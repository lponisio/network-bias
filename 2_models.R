rm(list=ls())
source("~/lab_paths.R")
local.path
setwd(local.path)

library(datawizard)
library(emmeans)
library(dplyr)
library(knitr)
library(kableExtra)
library(lmerTest)

setwd("network-bias")
source("src/initialize_packages.R")
source("src/initalize_models.R")

# =========================================================
# Country-level Variables — GLM Setup & Preprocessing
#
# To test whether different country-level variables affected the
# number of networks, we fit a GLM with number of networks collected
# in each country as a response variable and country area (km$^2$),
# research investment, and the number of bee species (the major
# pollinator group in most networks) as explanatory variables. We
# included an interaction with each of these variables and continent
# to allow their slopes to vary by continent.
# =========================================================
## Drop countries considered part of global oceans because it is not a
## distinguishable continent
webs_complete <- webs_complete[!webs_complete$Continent 
                               %in% c("Seven seas (open ocean)"), ]

webs_complete$Continent <- factor(webs_complete$Continent,
                                  levels=c("South America",
                                           "North America",
                                           "Africa",
                                           "Europe",
                                           "Asia",
                                           "Oceania"))

## Drop countries without complete data
webs_country <- webs_complete %>%
  distinct(adm0_a3, .keep_all = TRUE) %>%
  filter(!is.na(Continent),
         !is.na(AREA) , 
         !is.na(ResInvs_Density) ,
         !is.na(CL_Species_Density))

# Countries without any networks collected
no_webs <- webs_country[is.na(webs_country$Web_Code), ]
# Create LaTeX table
latex_table_no_webs <- make_latex_country_table(sort(no_webs$Country))
# Save as a text file
writeLines(latex_table_no_webs, "../network-bias-saved/manuscript/tables/latex_table_no_webs.txt")

# Webs excluded from analysis due to not meeting 1+ inclusion criteria
not_in_analysis <- setdiff(unique(webs_complete$NAME),
                           unique(webs_country$NAME))
# Create LaTeX table
latex_table_not_in_analysis <- make_latex_country_table(
  sort(not_in_analysis, decreasing = FALSE)
)
# Save as a text file
writeLines(latex_table_not_in_analysis, "../network-bias-saved/manuscript/tables/latex_table_not_in_analysis.txt")

# =========================================================
# Exclusions & Predictor Standardization — Diagnostics
#
# - Standardize country-level predictors (log-transform + z-score)
#   and visualize distributions for quick diagnostics.
# =========================================================
# Standardize variables and visualize their distributions
webs_country$log_AREA <-
  datawizard::standardize(log(webs_country$AREA))
webs_country$log_PropGDP_median <-
  datawizard::standardize(log(webs_country$PropGDP_median))
webs_country$log_CL_Species <-
  datawizard::standardize(log(webs_country$CL_Species))
webs_country$log_CL_Species_density <-
  datawizard::standardize(log(webs_country$CL_Species_Density))
webs_country$log_ResInvs_Density <-
  datawizard::standardize(log(webs_country$ResInvs_Density))

hist(webs_country[webs_country$Continent=="Africa",]$Total_webs_by_country)
hist(log(webs_country$Total_webs_by_country + 1))
hist(webs_country$log_ResInvs_Density)
hist(webs_country$log_CL_Species_density)
# =========================================================
# Negative Binomial GLM — Country-level Predictors of Network Counts
#
# - Test whether national predictors explain the number of published networks.
#
# Model:
# - Response: Total_webs_by_country (counts)
# - Family: Negative binomial (glm.nb) to handle overdispersion.
# - Predictors:
#   Continent (factor)
#   log_ResInvs_Density (R&D investment per km²; standardized log)
#   log_AREA (country area; standardized log)
#   log_CL_Species_density (bee richness per km²; standardized log)
# =========================================================
### -------------------------------
### Negative binomial model
### -------------------------------

network_use <- glm.nb(
  Total_webs_by_country ~ Continent +
    log_ResInvs_Density +
    log_AREA +
    log_CL_Species_density,
  data = webs_country
)

# Model summary + R2
summary(network_use)
r2_vals <- performance::r2(network_use)


### -------------------------------
### Model diagnostics
### -------------------------------

check_nb <- check_model(network_use)

png(file.path(savefilepath, "figures", "modelChecks", "check_nb.png"),
    width = 1200, height = 800)
plot(check_nb)
dev.off()


### -------------------------------
### Pairwise continent contrasts
### -------------------------------

# Estimated marginal means for continents
emm_continent_network_use <- emmeans(network_use, specs = "Continent")

# Pairwise contrasts (Tukey adjusted)
pairwise_continent_network_use <- contrast(
  emm_continent_network_use,
  method = "pairwise",
  adjust = "tukey"
)

# Convert to dataframe
pairwise_df_network_use <- as.data.frame(pairwise_continent_network_use)

# Save contrasts to CSV
write.csv(
  pairwise_df_network_use,
  file.path(savefilepath, "tables/network_use_mod_pairwise_continent.csv"),
  row.names = FALSE
)


### -------------------------------
### Create LaTeX table (custom function)
### -------------------------------

save_pairwise_latex(
  df = pairwise_df_network_use,
  file_path = file.path(savefilepath, "tables/pairwise_df_network_use.txt"),
  caption = "Pairwise comparisons of continents"
)


### -------------------------------
### Format GLM summary table
### -------------------------------

table_country <- format_glm_table(
  model = network_use,
  caption = "country.mod_continent"
)
# =========================================================
# Network Reuse — GLMM with Time Since Publication & Country
#
# Test whether the network re-use was related to the
# country from which the original study collected the network or
# simply the number of years since it has been published, we fit a
# GLMM with the number of times a network was re-used as the response
# variable, and time since the original paper was published and the
# country as explanatory variables. We included study as a random
# effect.
# =========================================================
# ============================================================
# Data Preparation
# ============================================================

# Filter out missing values
webs_reuse_wOutlier <- webs_complete %>%
  filter(
    !is.na(years_since_pub),
    !is.na(pub_count),
    !is.na(Continent)
  )

# Identify outliers (years > 60)
outlier_points <- webs_reuse_wOutlier %>%
  filter(years_since_pub > 60)

# Dataset with outliers removed
webs_reuse <- webs_reuse_wOutlier %>%
  filter(years_since_pub <= 60)

# Standardize and transform responses
add_transforms <- function(df) {
  df %>%
    mutate(
      log_years_since_pub = standardize(years_since_pub),
      log_pub_count       = log(pub_count + 0.01)
    )
}

webs_reuse_wOutlier <- add_transforms(webs_reuse_wOutlier)
webs_reuse          <- add_transforms(webs_reuse)


# ============================================================
# Diagnostic Plot: Outliers
# ============================================================

p_outliers <- ggplot(webs_reuse_wOutlier,
                     aes(x = years_since_pub, y = pub_count)) +
  geom_point(aes(color = years_since_pub > 60),
             size = 3, alpha = 0.7) +
  geom_text(
    data = outlier_points,
    aes(label = Web_Code),
    vjust = -2.5, hjust = 0.9, size = 3
  ) +
  scale_color_manual(
    values = c("FALSE" = "blue", "TRUE" = "red"),
    labels = c("Included", "Outlier")
  ) +
  labs(
    title = "Network Reuse Diagnostic: Highlighting Outliers",
    x = "Years Since Publication",
    y = "Number of Times Network Reused",
    color = "Point Type"
  ) +
  theme_bw()

print(p_outliers)

ggsave(
  file.path(savefilepath, "figures", "modelChecks",
            "diagnostic_outliers.pdf"),
  plot = p_outliers, width = 8, height = 6
)


# ============================================================
# Fit GLMMs (With & Without Outliers)
# ============================================================

reuse_formula <- log_pub_count ~ Continent * log_years_since_pub +
  (1 | Web_Code_base)

reuse_mod_wOutlier <- lmer(reuse_formula, data = webs_reuse_wOutlier)
reuse_mod          <- lmer(reuse_formula, data = webs_reuse)

# Diagnostics
check_reuse_wOutlier <- check_model(reuse_mod_wOutlier)
check_reuse          <- check_model(reuse_mod)

png(file.path(savefilepath, "figures", "modelChecks",
              "check_reuse_wOutlier.png"),
    width = 1200, height = 800)
plot(check_reuse_wOutlier)
dev.off()

png(file.path(savefilepath, "figures", "modelChecks",
              "check_reuse.png"),
    width = 1200, height = 800)
plot(check_reuse)
dev.off()


# ============================================================
# Model Output Tables (With & Without Outliers)
# ============================================================

save_coef_table <- function(model, outpath) {
  coefs   <- tidy(model, effects = "fixed", conf.int = TRUE)
  r2_vals <- performance::r2(model)
  
  table_out <- coefs %>%
    mutate(
      marginal_R2    = r2_vals$R2_marginal,
      conditional_R2 = r2_vals$R2_conditional
    )
  
  write.csv(table_out, outpath, row.names = FALSE)
}

save_coef_table(
  reuse_mod,
  "../network-bias-saved/manuscript/tables/reuse_model_table.csv"
)

save_coef_table(
  reuse_mod_wOutlier,
  "../network-bias-saved/manuscript/tables/reuse_model_table_wOutlier.csv"
)


# ============================================================
# Export Formatted Tables for Manuscript
# ============================================================

table_reuse <- format_lmer_table(
  model   = reuse_mod,
  caption = "webs_reuse_mod"
)

table_reuse_wOutlier <- format_lmer_table_new(
  model   = reuse_mod_wOutlier,
  caption = "webs_reuse_mod_wOutlier"
)


# ============================================================
# Likelihood Ratio Test: Global Effect of Continent
# ============================================================

reuse_mod_reduced <- lmer(
  log_pub_count ~ log_years_since_pub + (1 | Web_Code_base),
  data = webs_reuse
)

lrt_continent <- anova(reuse_mod_reduced, reuse_mod)
print(lrt_continent)

write.csv(
  as.data.frame(lrt_continent),
  "../network-bias-saved/manuscript/tables/reuse_mod_LRT_continent.csv",
  row.names = FALSE
)


# ============================================================
# Estimated Marginal Means & Pairwise Comparisons
# ============================================================

# EMMs for Continent
emm_continent_reuse <- emmeans(reuse_mod, ~ Continent)

#this is adjusting the pvalues a lot... wondering why its so sensitve?
#need to dig into this deeper
# Pairwise contrasts
pairwise_continent_reuse <- contrast(
  emm_continent_reuse,
  method = "pairwise",
  adjust = "tukey"
)

pairwise_df_reuse <- as.data.frame(pairwise_continent_reuse)

write.csv(
  pairwise_df_reuse,
  "../network-bias-saved/manuscript/tables/reuse_mod_pairwise_continent.csv",
  row.names = FALSE
)

save_pairwise_latex(
  df = pairwise_df_reuse,
  file_path = file.path(savefilepath, "tables/pairwise_df_reuse.txt"),
  caption = "Pairwise comparisons of continents"
)


# Slopes by Continent
slopes <- emtrends(
  reuse_mod, ~ Continent,
  var = "log_years_since_pub"
)

pairwise_slopes <- pairs(slopes, adjust = "tukey")

write.csv(
  as.data.frame(pairwise_slopes),
  "../network-bias-saved/manuscript/tables/reuse_mod_pairwise_slopes.csv",
  row.names = FALSE
)



