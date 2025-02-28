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

library(ggplot2)
## library(ggeffects)
library(viridis)
#data
webs_complete <- read.csv("network-bias-saved/raw/saved/webs_complete.csv")
## ***********************************************
## Biome models
## ***********************************************
#For these models we are evaluating the total webs
#per biome. We are investigating how area of the biome
#affects the number of webs and if the hemisphere
#has an effect
## ***********************************************
#we now have 
webs_biome <- webs_complete %>%
  filter(BiomeCode != "NA")%>%
  distinct(BiomeCode,Hemisphere, .keep_all = TRUE) 

biome.net.m2 <- glm(Total_webs_by_biome_by_hemi ~ log(area_by_hemisphere) * Hemisphere,
                    data = webs_biome,
                    family = "poisson")
summary(biome.net.m2)

performance::check_model(biome.net.m2)

## ***********************************************

webs_biome$logArea <- log(webs_biome$area_by_hemisphere)

ggplot(webs_biome,
       aes(x = logArea, y = Total_webs_by_biome_by_hemi, color = Hemisphere)) +
  geom_point() +
  scale_color_viridis(discrete = T) +
  geom_smooth(method = "glm", se = T,
              method.args = list(family = "poisson")) +
  labs(x="Area per biome (log)", y="Networks") +
  theme_classic()


## ***********************************************
## GLMM Models #Struggling here
## ***********************************************
webs_country <- webs_complete %>%
  distinct(ISO3, .keep_all = TRUE) %>%
  filter(!is.na(CL_Species))

webs_country <- webs_country[webs_country$Continent != "Oceania",]
webs_country$Continent <- factor(webs_country$Continent,
                                     levels=c("Americas",
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

