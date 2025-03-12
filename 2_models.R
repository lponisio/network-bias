rm(list=ls())
## ***********************************************
#workingdirectory
source("~/lab_paths.R")
local.path
setwd(local.path)

#packages
library(performance)
library(lme4)
library(dplyr)
library(MASS)
library(cowplot)
library(ggplot2)
## library(ggeffects)
library(viridis)
#data
webs_complete <- read.csv("network-bias-saved/raw/saved/webs_complete.csv")

savefilepath <- c("network-bias-saved/manuscript/figures")

## ***********************************************
## Biome models
## ***********************************************
## from manuscript currently
# To test whether network collection is related to the biome area in the Northern 
# and Southern hemispheres, we included the number of networks by country as a 
# response variable and the biome area as an explanatory variable in a generalized 
# linear model (GLM).  We included an interaction between biome area and Northern 
# vs. Southern hemispehere to allow the accumulation of network with are to vary 
# by hemisphere. 
## ***********************************************
#we now have 
webs_bycounty <- webs_complete %>%
  filter(BiomeCode != "NA")%>%
  distinct(ISO3, .keep_all = TRUE) 

biome.net.m2 <- glm(Total_webs_by_country ~ log(AREA_biome_total) * Hemisphere,
                    data = webs_bycounty,
                    family = "poisson")
summary(biome.net.m2)

performance::check_model(biome.net.m2)

## ***********************************************
## country-level variables
## ***********************************************
#from manuscript
# Next, to test whether different country-level variables affected the number of 
# networks, we fit a GLM  with number of networks collected in each country as a 
# response variable and country area (km$^2$), research investment, and the number
# of bee species (the major pollinator group in most networks) as explanatory 
# variables. We included an interaction with each of these variables and continent 
# to allow their slopes to vary by continent.  
## ***********************************************
webs_country <- webs_complete %>%
  distinct(ISO3, .keep_all = TRUE) %>%
  filter(!is.na(CL_Species))

webs_country <- webs_country[webs_country$Continent != "Oceania",]

#Idk if we want this? can update in cleaning pipeline if so
webs_country <- webs_country %>%
  mutate(Continent = ifelse(Continent == "Americas" & Hemisphere == "Northern", 
                            "Northern America", 
                            ifelse(Continent == "Americas" & Hemisphere == "Southern", 
                                   "Southern America", 
                                   Continent)))

webs_country$Continent <- factor(webs_country$Continent,
                                     levels=c("Northern America",
                                              "Southern America",
                                              "Africa",
                                              "Europe",
                                              "Asia"))

# Add a small constant (1)
webs_country$log_CL_Species <- log(webs_country$CL_Species + 1)

library(glmmTMB)
library(car)

country.mod_continent <- glm(Total_webs_by_country ~ scale(log(AREA)) +
                                   scale(log_CL_Species) + Continent +
                                scale(PropGDP_median),
                                 data=webs_country,
                                 ## ziformula = ~1,
                                 family = "poisson")

summary(country.mod_continent)
vif(country.mod_continent)

performance::check_model(country.mod_continent)

# Residuals plot
residuals <- residuals(country.mod_continent)
plot(residuals)

# Predicted vs. residuals
plot(predict(country.mod_continent), residuals)

## ***********************************************
## network re-use
## ***********************************************
#from manuscript
# Lastly, to test whether the network re-use was related to the country from 
# which the original study collected the network or simply the number of years 
# since it has been published, we fit a GLMM with the number of times a network 
# was re-used as the response variable, and time since the original paper was 
# published and the country as explanatory variables. Similar to the second model, 
# we included continent as a random effect. 
## ***********************************************
webs_reuse <- webs_complete %>%
  distinct(Web_Code, .keep_all = TRUE) %>%
  mutate(years_since_pub = as.numeric(format(Sys.Date(), "%Y")) - Publi_Year)

#ARG is the intercept :)
webs_reuse_mod <- glm(Use_Frequency ~ ISO3 +years_since_pub,
                             data=webs_reuse,
                             ## ziformula = ~1,
                             family = "poisson")

summary(country.mod_continent)

performance::check_model(country.mod_continent)


# Residuals plot
residuals <- residuals(country.mod_continent)
plot(residuals)
# Predicted vs. residuals
plot(predict(country.mod_continent), residuals)








