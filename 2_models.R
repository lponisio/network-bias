rm(list=ls())
source("~/lab_paths.R")
local.path
setwd(local.path)

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
                                  levels=c("North America",
                                           "South America",
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
# Negative binomial model 
network_use <- glm.nb(Total_webs_by_country ~ Continent+
                        log_ResInvs_Density +
                        log_AREA +
                        log_CL_Species_density,
                      data = webs_country)

check_nb <-check_model(network_use)
summary(network_use)
r2_vals <- performance::r2(network_use)

# Save plot outputs
png("../network-bias-saved/manuscript/figures/modelChecks/check_nb.png", width=1200, height=800)
plot(check_nb)
dev.off()

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
# Filter to usable records for reuse analysis (remove NAs; cap years_since_pub)
webs_reuse_wOutlier <- webs_complete %>%
  filter(!is.na(years_since_pub),
         !is.na(pub_count),
         !is.na(Continent))

#check for extreme outliers
hist(webs_reuse_wOutlier$years_since_pub)
webs_reuse_wOutlier[webs_reuse_wOutlier$years_since_pub>60,]$Web_Code
webs_reuse_wOutlier[webs_reuse_wOutlier$years_since_pub>60,]$years_since_pub
#remove outlier, do analysis for both
webs_reuse <- webs_reuse_wOutlier[webs_reuse_wOutlier$years_since_pub<60,]
  
# Identify Web_Code present in complete dataset but not in reuse subset
not_in_analysis_wOutlier <-
  setdiff(unique(webs_complete$Web_Code), unique(webs_reuse_wOutlier$Web_Code))
not_in_analysis <-
  setdiff(unique(webs_complete$Web_Code), unique(webs_reuse$Web_Code))

# Keep metadata rows for those excluded codes
not_in_analysis_wOutlier <- webs_complete %>%
  distinct(Web_Code, .keep_all = TRUE) %>%
  filter(Web_Code %in%not_in_analysis_wOutlier)

not_in_analysis <- webs_complete %>%
  distinct(Web_Code, .keep_all = TRUE) %>%
  filter(Web_Code %in%not_in_analysis)

## Summary stats for manuscript
# Reuse count summary (mean, SD) and mean time since publication
mean(webs_reuse_wOutlier$pub_count)
sd(webs_reuse_wOutlier$pub_count)
mean(webs_reuse_wOutlier$years_since_pub)

mean(webs_reuse$pub_count)
sd(webs_reuse$pub_count)
mean(webs_reuse$years_since_pub)

# Standardize time since publication; log-transform reuse count (with small offset)
webs_reuse_wOutlier$log_years_since_pub <-
  datawizard::standardize(webs_reuse_wOutlier$years_since_pub)
webs_reuse_wOutlier$log_pub_count <- log(webs_reuse_wOutlier$pub_count + .01)

webs_reuse$log_years_since_pub <-
  datawizard::standardize(webs_reuse$years_since_pub)
webs_reuse$log_pub_count <- log(webs_reuse$pub_count + .01)

# GLMM model
reuse_mod_wOutlier <- lmerTest::lmer(log_pub_count ~
                              Continent * log_years_since_pub +
                              (1 | Web_Code_base),
                            data = webs_reuse_wOutlier)

reuse_mod <- lmerTest::lmer(log_pub_count ~
                                       Continent * log_years_since_pub +
                                       (1 | Web_Code_base),
                                     data = webs_reuse)

# Model diagnostics and summary
check_reuse_wOutlier<-check_model(reuse_mod_wOutlier)
summary(reuse_mod_wOutlier)

# Model diagnostics and summary
check_reuse<-check_model(reuse_mod)
summary(check_reuse)

# Save plot outputs
png("../network-bias-saved/manuscript/figures/modelChecks/check_reuse_wOutlier.png", width=1200, height=800)
plot(check_reuse_wOutlier)
dev.off()
png("../network-bias-saved/manuscript/figures/modelChecks/check_reuse.png", width=1200, height=800)
plot(check_reuse)
dev.off()

# =========================================================
#without Outlier
coefs <- tidy(reuse_mod, effects = "fixed", conf.int = TRUE)
r2_vals <- performance::r2(reuse_mod)
table_out <- coefs %>%
  mutate(
    marginal_R2   = r2_vals$R2_marginal,
    conditional_R2 = r2_vals$R2_conditional
  )
# Write to CSV
write.csv(table_out, "../network-bias-saved/manuscript/tables/reuse_model_table.csv", row.names = FALSE)

#with Outlier
coefs <- tidy(reuse_mod_wOutlier, effects = "fixed", conf.int = TRUE)
r2_vals <- performance::r2(reuse_mod_wOutlier)
table_out_wOutlier <- coefs %>%
  mutate(
    marginal_R2   = r2_vals$R2_marginal,
    conditional_R2 = r2_vals$R2_conditional
  )
# Write to CSV
write.csv(table_out_wOutlier, "../network-bias-saved/manuscript/tables/reuse_model_table_wOutlier.csv", row.names = FALSE)

# =========================================================
# Export formatted tables for manuscript (GLM and LMM)
table_country <- format_glm_table(
  network_use,
  caption = "country.mod_continent"
)
table_reuse <- format_lmer_table(
  reuse_mod,
  caption = "webs_reuse_mod"
)

table_reuse <- format_lmer_table_new(
  reuse_mod_wOutlier,
  caption = "webs_reuse_mod_wOutlier"
)
