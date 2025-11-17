rm(list=ls())
source("~/lab_paths.R")
local.path
setwd(local.path)

setwd("network-bias")
source("src/initialize_packages.R")
source("src/initalize_models.R")

# =========================================================
# Network & Author Summaries for Manuscript
# =========================================================

## Deduplicate networks by Web_Code and drop missing codes
webs <- webs_complete%>%
  distinct(Web_Code, .keep_all = TRUE)
webs<-webs[!is.na(webs$Web_Code),]

## Parse first author from Web_Code pattern "Author_Year_..."
webs$FirstAuthor <- sapply(strsplit(webs$Web_Code, "_"),
                           function(x) x[1])

# -------------------------------------------------------
# Summary stats for manuscript
# -------------------------------------------------------

# Countries with data by continent (subset and percentages by adm0_a3)
Africa <- webs[webs$Continent=="Africa",]

Africa_summary <- Africa %>%
  count(adm0_a3) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  arrange(desc(percent)) 
unique(Africa$Web_Code_base)

##  Get the top 6 countries by number of networks
top_countries <- webs %>%
  count(adm0_a3, name = "n_networks") %>%
  mutate(percent = 100 * n_networks / sum(n_networks)) %>%
  arrange(desc(n_networks)) %>%
  slice_head(n = 6) 
top_countries


# Count top 3 first authors within each of those countries
top_authors_by_country <- webs %>%
  filter(adm0_a3 %in% top_countries$adm0_a3) %>%
  count(adm0_a3, FirstAuthor, Continent,  name = "n_networks") %>%
  group_by(adm0_a3, Continent) %>%
  arrange(desc(n_networks)) %>%
  slice_head(n = 3) %>%
  ungroup()
top_authors_by_country

# Count top 5 first authors in Southern Hemisphere countries (Global South proxy)
top_authors_global_S <- webs %>%
  filter(Hemisphere == "Southern") %>%
  count(adm0_a3, Continent, FirstAuthor, name = "n_networks") %>%
  group_by(adm0_a3, Continent) %>%
  arrange(desc(n_networks)) %>%
  slice_head(n = 5) %>%
  ungroup()
top_authors_global_S

##  Calculate percent area of top 6 countries (relative to global AREA sum)
percent_area_top6 <- webs %>%
  distinct(adm0_a3, .keep_all = TRUE) %>%
  summarise(
    total_area_all = sum(AREA, na.rm = TRUE),
    total_area_top6 = sum(AREA[adm0_a3 %in% top_countries$adm0_a3],
                          na.rm = TRUE)
  ) %>%
  mutate(percent_area = total_area_top6 / total_area_all)

print(percent_area_top6$percent_area)

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


top6 <- webs_country %>%
  filter(adm0_a3 %in% top_countries$adm0_a3) %>%
  select(adm0_a3, Networks = Total_webs_by_country, Land_area = AREA)


## Countries without any networks collected## CouTRUEntries without any networks collected
no_webs <- webs_country[is.na(webs_country$Web_Code),]
sort(no_webs$Country)
latex_table <- make_latex_country_table(no_webs$Country)

# =========================================================
# Exclusions & Predictor Standardization — Diagnostics
#
# - Standardize country-level predictors (log-transform + z-score)
#   and visualize distributions for quick diagnostics.
# =========================================================

# Webs excluded from analysis due to not meeting 1+ inclusion criteria
not_in_analysis <- setdiff(unique(webs_complete$NAME),
                           unique(webs_country$NAME))

latex_table <- make_latex_country_table(sort(not_in_analysis,
                                             decreasing =FALSE))
cat(latex_table)

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
png("model_checks/check_nb.png", width=1200, height=800)
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
webs_reuse <- webs_reuse_wOutlier[webs_reuse_wOutlier$years_since_pub<60,]
  
## Need to only be for actual web_cobe networks
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

# Save plot outputs
png("model_checks/check_reuse_wOutlier.png", width=1200, height=800)
plot(check_reuse_wOutlier)
#plot(reuse_mod_wOutlier)
dev.off()
png("model_checks/check_reuse.png", width=1200, height=800)
plot(check_reuse)
#plot(reuse_mod_wOutlier)
dev.off()


# Model diagnostics and summary
check_reuse <-check_model(reuse_mod)
summary(reuse_mod)
# =========================================================
library(broom.mixed)

#without Outlier
coefs <- tidy(reuse_mod, effects = "fixed", conf.int = TRUE)
r2_vals <- performance::r2(reuse_mod)
table_out <- coefs %>%
  mutate(
    marginal_R2   = r2_vals$R2_marginal,
    conditional_R2 = r2_vals$R2_conditional
  )
# Write to CSV
write.csv(table_out, "model_checks/reuse_model_table.csv", row.names = FALSE)

#with Outlier
coefs <- tidy(reuse_mod_wOutlier, effects = "fixed", conf.int = TRUE)
r2_vals <- performance::r2(reuse_mod_wOutlier)
table_out_wOutlier <- coefs %>%
  mutate(
    marginal_R2   = r2_vals$R2_marginal,
    conditional_R2 = r2_vals$R2_conditional
  )
# Write to CSV
write.csv(table_out_wOutlier, "model_checks/reuse_model_table_wOutlier.csv", row.names = FALSE)

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
