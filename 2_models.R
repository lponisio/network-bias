############################################################
# 1. SETUP: Paths, Packages, and Initialization
############################################################

rm(list = ls())
source("~/lab_paths.R")
setwd(local.path)

library(dplyr)
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
         !is.na(ResInvs_Density_2) ,
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
webs_country$log_CL_Species <-
  datawizard::standardize(log(webs_country$CL_Species))
webs_country$log_CL_Species_density <-
  datawizard::standardize(log(webs_country$CL_Species_Density))
webs_country$log_ResInvs_Density_2 <-
  datawizard::standardize(log(webs_country$ResInvs_Density_2))
webs_country$log_ResInvs_Density_5 <-
  datawizard::standardize(log(webs_country$ResInvs_Density_5))

hist(webs_country[webs_country$Continent=="Africa",]$Total_webs_by_country)
hist(log(webs_country$Total_webs_by_country + 1))

hist(webs_country$log_ResInvs_Density_2)
hist(webs_country$log_ResInvs_Density_5)
hist(webs_country$log_CL_Species_density)

# This negative binomial model accounts for dispersion
network_use_2 <- glm.nb(Total_webs_by_country ~ Continent+
                        log_ResInvs_Density_2 +
                        log_AREA +
                        log_CL_Species_density,
                      data = webs_country)

summary(network_use_2)
check_model(network_use_2)
r2_vals <- performance::r2(network_use_2)

# Diagnostics
check_nb_2 <- check_model(network_use_2)
png(file.path(savefilepath, "figures", "modelChecks", "check_nb_2.png"),
    width = 1200, height = 800)
plot(check_nb_2)
dev.off()

# GLM table for manuscript
table_country_2 <- format_glm_table(
  model   = network_use_2,
  caption = "country.mod_continent_2"
)


# This negative binomial model accounts for dispersion
network_use_5 <- glm.nb(Total_webs_by_country ~ Continent+
                          log_ResInvs_Density_5 +
                          log_AREA +
                          log_CL_Species_density,
                        data = webs_country)

summary(network_use_5)
check_model(network_use_5)
r2_vals <- performance::r2(network_use_5)

# Diagnostics
check_nb_5 <- check_model(network_use_5)
png(file.path(savefilepath, "figures", "modelChecks", "check_nb_5.png"),
    width = 1200, height = 800)
plot(check_nb_5)
dev.off()

# GLM table for manuscript
table_country_5 <- format_glm_table(
  model   = network_use_5,
  caption = "country.mod_continent_5"
)

############################################################
# 3. NETWORK REUSE ANALYSIS
#    Does reuse depend on continent & publication age?
############################################################

# ----------------------------------------------------------
# Data preparation
# ----------------------------------------------------------

webs_reuse <- webs_complete %>%
  filter(
    !is.na(years_since_pub),
    !is.na(pub_count),
    !is.na(Continent)
  )

# Outlier identification (years > 60)
outlier_points <- webs_reuse %>%
  filter(years_since_pub > 60)

p_outliers <- ggplot(webs_reuse,
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
    color = "over 60 years"
  ) +
  theme_bw()

ggsave(file.path(savefilepath, "figures", "modelChecks","diagnostic_outliers.pdf"),p_outliers, width = 8, height = 6)


# Remove outliers and standardize the data
webs_reuse <- webs_reuse %>%
  filter(years_since_pub <= 60)%>%
  mutate(
    log_years_since_pub = datawizard::standardize(years_since_pub),
    log_pub_count       = log(pub_count + 0.01)
  )

# ----------------------------------------------------------
# 3.2 Fit GLMMs (with and without outliers)
# ----------------------------------------------------------

reuse_mod<- lmer(log_pub_count ~ Continent * log_years_since_pub + (1 | Web_Code_base), data = webs_reuse)

#saving output
summary(reuse_mod)
check_reuse <-check_model(reuse_mod)
r2_vals <- performance::r2(reuse_mod)

png(file.path(savefilepath, "figures", "modelChecks", "check_reuse.png"),
    width = 1200, height = 800)
plot(check_reuse)
dev.off()

table_reuse <- format_lmer_table(
  model   = reuse_mod,
  caption = "check_reuse.png"
)



library(influence.ME)

infl <- influence(reuse_mod, obs = TRUE)   # obs=TRUE gives observation-level influence

dfb <- dfbetas(infl)

plot(dfb[, "log_years_since_pub"], type = "h")
abline(h = c(-0.5, 0.5), lty = 2, col = "red")

dfb_slope <- dfb[, "log_years_since_pub"]
infl_idx <- which(abs(dfb_slope) > 0.5)

infl_points <- webs_reuse[infl_idx, ]

infl_points[, c("Web_Code_base", "years_since_pub", "pub_count", "Continent")]


# ----------------------------------------------------------
# 3.3 Likelihood ratio test for continent
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



