############################################################
# 1. SETUP: Paths, Packages, and Initialization
############################################################

rm(list = ls())
source("~/lab_paths.R")
setwd(local.path)

library(dplyr)
library(datawizard)
library(emmeans)
library(knitr)
library(kableExtra)
library(lmerTest)

# Project-specific initializations
setwd("network-bias")
source("src/initialize_packages.R")
source("src/initalize_models.R")

############################################################
# 2. COUNTRY-LEVEL ANALYSIS
#    Do country-level variables predict number of networks?
############################################################

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

## Countries without any networks collected
no_webs <- webs_country[is.na(webs_country$Web_Code),]
sort(no_webs$Country)

## ***********************************************************
## Webs excluded from analysis due to not meeting 1+ inclusion
## criteria

# Countries with zero networks (for manuscript tables)
no_webs <- webs_country[is.na(webs_country$Web_Code), ]
writeLines(
  make_latex_country_table(sort(no_webs$Country)),
  "../network-bias-saved/manuscript/tables/latex_table_no_webs.txt"
)

# Countries excluded due to incomplete data
not_in_analysis <- setdiff(unique(webs_complete$NAME),
                           unique(webs_country$NAME))

writeLines(
  make_latex_country_table(sort(not_in_analysis)),
  "../network-bias-saved/manuscript/tables/latex_table_not_in_analysis.txt"
)

## ***********************************************************
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

# This negative binomial model accounts for dispersion
network_use <- glm.nb(Total_webs_by_country ~ Continent+
                        log_ResInvs_Density +
                        log_AREA +
                        log_CL_Species_density,
                      data = webs_country)

summary(network_use)
check_model(network_use)
r2_vals <- performance::r2(network_use)

# Diagnostics
check_nb <- check_model(network_use)
png(file.path(savefilepath, "figures", "modelChecks", "check_nb.png"),
    width = 1200, height = 800)
plot(check_nb)
dev.off()

# GLM table for manuscript
table_country <- format_glm_table(
  model   = network_use,
  caption = "country.mod_continent"
)


############################################################
# 3. NETWORK REUSE ANALYSIS
#    Does reuse depend on continent & publication age?
############################################################

# ----------------------------------------------------------
# 3.1 Data preparation
# ----------------------------------------------------------

webs_reuse_wOutlier <- webs_complete %>%
  filter(
    !is.na(years_since_pub),
    !is.na(pub_count),
    !is.na(Continent)
  )

# Outlier identification (years > 60)
outlier_points <- webs_reuse_wOutlier %>%
  filter(years_since_pub > 60)

# Outliers removed dataset
webs_reuse <- webs_reuse_wOutlier %>%
  filter(years_since_pub <= 60)

# Standardize variables
add_transforms <- function(df) {
  df %>%
    mutate(
      log_years_since_pub = standardize(years_since_pub),
      log_pub_count       = log(pub_count + 0.01)
    )
}

webs_reuse          <- add_transforms(webs_reuse)
webs_reuse_wOutlier <- add_transforms(webs_reuse_wOutlier)


# ----------------------------------------------------------
# 3.2 Diagnostic plot for outliers
# ----------------------------------------------------------

p_outliers <- ggplot(webs_reuse_wOutlier,
                     aes(years_since_pub, pub_count)) +
  geom_point(aes(color = years_since_pub > 60),
             size = 3, alpha = 0.7) +
  geom_text(
    data  = outlier_points,
    aes(label = Web_Code),
    vjust = -2.5, hjust = 0.9, size = 3
  ) +
  scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
  labs(
    title = "Network Reuse Diagnostic: Highlighting Outliers",
    x = "Years Since Publication",
    y = "Times Network Reused",
    color = "Point Type"
  ) +
  theme_bw()

ggsave(
  file.path(savefilepath, "figures", "modelChecks",
            "diagnostic_outliers.pdf"),
  p_outliers, width = 8, height = 6
)


# ----------------------------------------------------------
# 3.3 Fit GLMMs (with and without outliers)
# ----------------------------------------------------------

reuse_formula <- log_pub_count ~ Continent * log_years_since_pub +
  (1 | Web_Code_base)

reuse_mod_wOutlier <- lmer(reuse_formula, data = webs_reuse_wOutlier)
reuse_mod          <- lmer(reuse_formula, data = webs_reuse)

# Diagnostics
for (model_name in c("reuse_mod_wOutlier", "reuse_mod")) {
  png(file.path(
    savefilepath, "figures", "modelChecks",
    paste0("check_", model_name, ".png")
  ),
  width = 1200, height = 800)
  plot(check_model(get(model_name)))
  dev.off()
}


# ----------------------------------------------------------
# 3.5 Formatted model output tables
# ----------------------------------------------------------

table_reuse <- format_lmer_table(
  model   = reuse_mod,
  caption = "webs_reuse_mod_Asia"
)

table_reuse_wOutlier <- format_lmer_table_new(
  model   = reuse_mod_wOutlier,
  caption = "webs_reuse_mod_wOutlier"
)


# ----------------------------------------------------------
# 3.6 Likelihood ratio test for continent
# ----------------------------------------------------------

reuse_mod_reduced <- lmer(
  log_pub_count ~ log_years_since_pub + (1 | Web_Code_base),
  data = webs_reuse
)

lrt_continent <- anova(reuse_mod_reduced, reuse_mod)
write.csv(
  as.data.frame(lrt_continent),
  "../network-bias-saved/manuscript/tables/reuse_mod_LRT_continent.csv",
  row.names = FALSE
)



