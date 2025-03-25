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
webs_complete <- read.csv("network-bias-saved/saved/webs_complete.csv")

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

webs_bycounty$logArea <- log(webs_bycounty$AREA_biome_total+ 1)
webs_bycounty$logNetworksCountry <- log(webs_bycounty$Total_webs_by_country+ 1)

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
webs_country$logNetworksCountry <- log(webs_country$Total_webs_by_country+ 1)

area <- ggplot(webs_country,
               aes(x = log(AREA), y = logNetworksCountry, color = Continent)) +
  geom_point(show.legend = FALSE) +  # Remove legend from points
  scale_color_viridis(discrete = TRUE) +
  geom_smooth(method = "glm",
              method.args = list(family = "quasipoisson"), # Closest match to neg. binomial in ggplot
              formula = y ~ scale(x),
              se = TRUE, show.legend = FALSE) +
  scale_x_continuous(labels = function(x) round(exp(x), -2)) + # Convert log-scale to original
  scale_y_continuous(labels = function(x) round(exp(x))) + # Convert log-scale to original
  labs(x = "Area", y = "Networks") +
  theme_classic()

#research invs by networks
res_inv <- ggplot(webs_country,
                  aes(x = PropGDP_median, y = logNetworksCountry, color = Continent)) +
  geom_point() +
  scale_color_viridis(discrete = T) +
  geom_smooth(method = "glm",
              method.args = list(family = "quasipoisson"), # Closest match to neg. binomial in ggplot
              formula = y ~ scale(x),
              se = TRUE) +
  scale_y_continuous(labels = function(x) round(exp(x), -1)) + # Convert log-scale to original
  labs(x="Research Investment (PropGDP_median)", y="") +
  theme_classic()

#Bee species
bees <- ggplot(webs_country,
               aes(x = log_CL_Species, y = logNetworksCountry, color = Continent)) +
  geom_point(show.legend = FALSE) +
  scale_color_viridis(discrete = T) +
  geom_smooth(method = "glm",
              method.args = list(family = "quasipoisson"), # Closest match to neg. binomial in ggplot
              formula = y ~ scale(x),
              se = TRUE, show.legend = FALSE) +
  scale_x_continuous(labels = function(x) round(exp(x), -1)) + # Convert log-scale to original
  scale_y_continuous(labels = function(x) round(exp(x), -1)) + 
  labs(x="Bee Species", y="") +
  theme_classic()   # Move legend to bottom

library(patchwork)
# Combine the three plots with a centered legend
combined_plot <- (area + res_inv + bees) +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A') &
  theme(legend.position = "bottom", 
        legend.justification = "center")

ggsave(combined_plot, file = paste0(savefilepath, "/combined_plot.jpg"), height = 5, width = 12)

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
  filter(!is.na(Use_Frequency))%>%
  mutate(network_type = ifelse(Use_Frequency == 1, "Original", "Reused"))

# Summarize the count of original and reused networks by year
yearly_trend <- webs_reuse %>%
  group_by(Publi_Year, network_type) %>%
  summarise(paper_count = n(), .groups = "drop")

#line
ggplot(yearly_trend[(yearly_trend$Publi_Year > 2000),]) +
  geom_point(aes(x = Publi_Year, y = paper_count, color = network_type))+
  geom_line(aes(x = Publi_Year, y = paper_count, color = network_type))+
  theme_minimal() +
  labs(x = "Publication Year", y = "Total Publications") 
## ***********************************************

large <-ggplot(webs_reuse, 
       aes(x = years_since_pub, 
           y = Use_Frequency, 
           color = ISO3)) +
  geom_text(aes(label = ISO3), 
            position = position_jitter(width = 1.5, height = 1.5), 
            check_overlap = FALSE,
            show.legend = FALSE) +
  labs(
    x = "",         
    y = "Use Frequncy"                  
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 18),  
    axis.text = element_text(size = 18) 
  )
  
webs_medium <-webs_reuse[(webs_reuse$years_since_pub <25),]

medium <- ggplot(webs_medium,
       aes(x = years_since_pub, 
           y = Use_Frequency, 
           color = ISO3)) +
  geom_text(aes(label = ISO3), 
            size = 4,
            position = position_jitter(width = 1.5, height = 1.5), 
            check_overlap = FALSE,
            show.legend = FALSE) +
  labs(
    x = "",         
    y = ""                  
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 18),  
    axis.text = element_text(size = 18) 
  )

webs_small <-webs_reuse[(webs_reuse$years_since_pub <20),]

webs_small<-webs_small[(webs_small$Use_Frequency <10),]

small <-ggplot(webs_small,
       aes(x = years_since_pub, 
           y = Use_Frequency, 
           color = ISO3)) +
  geom_text(aes(label = ISO3), 
            size = 4,
            position = position_jitter(width = 1.5, height = 1.5), 
            check_overlap = FALSE,
            show.legend = FALSE) +
  labs(
    x = "Years Since Publication",         
    y = "Use Frequency"                  
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 18),  
    axis.text = element_text(size = 18) 
  )

webs_smallest <-webs_reuse[(webs_reuse$years_since_pub <10),]

webs_smallest <-webs_smallest[(webs_smallest$Use_Frequency <2.5),]

extra_small <- ggplot(webs_smallest,
       aes(x = years_since_pub, 
           y = Use_Frequency, 
           color = ISO3)) +
  geom_text(aes(label = ISO3), 
            size = 4,
            position = position_jitter(width = 1.5, height = 1.5), 
            check_overlap = FALSE,
            show.legend = FALSE) +
  labs(
    x = "Years Since Publication",         
    y = ""                  
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 18),  
    axis.text = element_text(size = 18) 
  )


library(patchwork)
# Combine the three plots with a centered legend
combined_plot_reuse <- (large + medium + small +extra_small) +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A') 

ggsave(combined_plot_reuse, file = paste0(savefilepath, "/combined_plot_reuse.jpg"), height = 20, width = 20)


## ***********************************************

#boxplot
ggplot(webs_reuse, aes(x = factor(years_since_pub), y = Use_Frequency)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "years_since_pub", y = "Data Reuse Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#heat map
ggplot(webs_reuse, aes(x = years_since_pub, y = ISO3, fill = Use_Frequency)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma")  +
  theme_minimal() +
  labs(x = "Years Since Publication", y = "Country", fill = "Data Reuse Count")

