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
## Biome figure
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

webs_bycounty$logArea <- log(webs_bycounty$AREA_biome_total)
webs_bycounty$logNetworksCountry <- log(webs_bycounty$Total_webs_by_country)

## ***********************************************

biome <- ggplot(webs_bycounty,
       aes(x = logArea, y = logNetworksCountry, color = Hemisphere)) +
  geom_point() +
  scale_color_viridis(discrete = T) +
  geom_smooth(method = "glm", se = T,
              method.args = list(family = "poisson")) +
  labs(x="Area per biome (log)", y="Networks") +
  theme_classic()


ggsave(biome, file = paste0(savefilepath, "/biome.jpg"), height = 10, width = 10)

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

## ***********************************************
webs_country$logNetworksCountry <- log(webs_country$Total_webs_by_country)

#area of country by networks
area <- ggplot(webs_country,
               aes(x = log(AREA), y = logNetworksCountry, color = Continent)) +
  geom_point() +
  scale_color_viridis(discrete = T) +
  geom_smooth(method = "glm", se = T,
              method.args = list(family = "poisson")) +
  labs(x="Country Area (log)", y="Networks (log)") +
  theme_classic()

#research invs by networks
res_inv <- ggplot(webs_country,
                  aes(x = PropGDP_median, y = logNetworksCountry, color = Continent)) +
  geom_point() +
  scale_color_viridis(discrete = T) +
  geom_smooth(method = "glm", se = T,
              method.args = list(family = "poisson")) +
  labs(x="Research Investment (PropGDP_median)", y="Networks (log)") +
  theme_classic()

#Bee species
bees <- ggplot(webs_country,
               aes(x = log_CL_Species, y = logNetworksCountry, color = Continent)) +
  geom_point() +
  scale_color_viridis(discrete = T) +
  geom_smooth(method = "glm", se = T,
              method.args = list(family = "poisson")) +
  labs(x="Bee Species (log)", y="Networks (log)") +
  theme_classic()

# Arrange plots
combined_plot <- plot_grid(area, res_inv, bees, ncol = 1,labels="AUTO")

ggsave(combined_plot, file = paste0(savefilepath, "/combined_plot.jpg"), height = 10, width = 10)

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


#boxplot
ggplot(webs_reuse, aes(x = reorder(ISO3, Use_Frequency, median), y = Use_Frequency)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Country", y = "Data Reuse Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#heat map
ggplot(webs_reuse, aes(x = years_since_pub, y = ISO3, fill = Use_Frequency)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma")  +
  theme_minimal() +
  labs(x = "Years Since Publication", y = "Country", fill = "Data Reuse Count")

