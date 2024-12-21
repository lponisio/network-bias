rm(list=ls())
## ***********************************************
#workingdirectory
source("~/lab_paths.R")
local.path

#packages
library(performance)
library(lme4)
library(dplyr)
library(MASS)

library(ggplot2)
library(ggeffects)
library(viridis)
#data
webs_complete <- read.csv("~/University of Oregon Dropbox/Rose McDonald/network-bias-saved/raw/saved/webs_complete.csv")
## ***********************************************
## ***********************************************
## Biome models
## ***********************************************
#For these models we are evaluating the total webs
#per biome. We are investigating how area of the biome
#affects the number of webs and if the hemisphere
#has an effect
## ***********************************************
webs_biome<- webs_complete %>%
  distinct(BiomeCode, .keep_all = TRUE) 

#Poisson looks awful
#honestly Gaussian looks really good too. 
biome.area.mod <- MASS::glm.nb(total_webs_global ~ log(total_area_global),
                      data=webs_biome)
summary(biome.area.mod)
performance::check_model(biome.area.mod)

## N hemiphere
biome.area.mod.N <- glm(NorthernWebs ~ log(total_area_north),
                        data=webs_biome, family="poisson")

summary(biome.area.mod.N)
check_model(biome.area.mod.N)


## S hemisphere
biome.area.mod.S <- glm(SouthernWebs ~ log(total_area_south+1),
                        data=webs_biome)

summary(biome.area.mod.S)
check_model(biome.area.mod.S)

#grouping a model with all biome data

biome.net.m1 <- glm(total_webs_global ~ log(total_area_global) + Hemisphere,
                    data = webs_biome,
                    family = "poisson")
summary(biome.net.m1)
check_model(biome.net.m1)

# model with hemisphere interaction
biome.net.m2 <- glm(total_webs_global ~ log(total_area_global) * Hemisphere,
                    data = webs_biome,
                    family = "poisson")
summary(biome.net.m2)
check_model(biome.net.m2)

## ***********************************************

webs_biome$logArea <- log(webs_biome$total_area_global)

ggplot(webs_biome,
       aes(x = logArea, y = total_webs_global, color = Hemisphere)) +
  geom_point() +
  scale_color_viridis(discrete = T) +
  geom_smooth(method = "glm", se = T,
              method.args = list(family = "poisson")) +
  labs(x="Area per biome (log)", y="Networks") +
  theme_classic()


## ***********************************************
## GLMM Models #Struggling here
## ***********************************************
webs_country<- webs_complete %>%
  distinct(Country.Code, .keep_all = TRUE) %>%
  filter(!is.na(Continent),
         !is.na(AREA),
         !is.na(CL_Species))

webs_country <- webs_country[webs_country$Continent != "Oceania",]
webs_country$Continent <- factor(webs_country$Continent,
                                     levels=c("Americas",
                                              "Africa",
                                              "Europe",
                                              "Asia"))
webs_country$log_CL_Species <- log(webs_country$CL_Species + 1)  # Add a small constant (1)


country.mod_continent <- zim(webs_country ~ log(AREA) + log_CL_Species + Continent, 
                             data = webs_country, 
                             dist = "zinb")
check_model(country.mod_continent)

# Residuals plot
residuals <- residuals(country.mod_continent)
plot(residuals)

# Predicted vs. residuals
plot(predict(country.mod_continent), residuals)

