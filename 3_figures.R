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
webs_complete <- webs_complete[webs_complete$Continent != "Oceania",]

webs_complete$Continent <- factor(webs_complete$Continent,
                                  levels=c("Northern America",
                                           "Southern America",
                                           "Africa",
                                           "Europe",
                                           "Asia"))


webs_country <- webs_complete %>%
  distinct(ISO3, .keep_all = TRUE) %>%
  filter(!is.na(Continent) & 
           !is.na(AREA) & 
           !is.na(PropGDP_median) & 
           !is.na(CL_Species))


# Ensure the dataset contains the required transformed variables
webs_country$log_AREA <- log(webs_country$AREA+1)
webs_country$log_PropGDP_median <- log(webs_country$PropGDP_median+1)
webs_country$log_CL_Species <- log(webs_country$CL_Species+1)
webs_country$log_Total_webs_by_country <- log(webs_country$Total_webs_by_country+1)

## ***********************************************

area <- ggplot(webs_country,
               aes(x = log_AREA, y = log_Total_webs_by_country, color = Continent)) +
  geom_point(show.legend = FALSE) +  # Remove legend from points
  scale_color_viridis(discrete = TRUE) +
  geom_smooth(method = "glm",
              method.args = list(family = "quasipoisson"), # Closest match to neg. binomial in ggplot
              formula = y ~ scale(x), alpha = 0.15,
              se = TRUE, show.legend = FALSE) +
  scale_x_continuous(labels = function(x) round(exp(x) -1, -2)) + # Convert log-scale to original
  scale_y_continuous(labels = function(x) round(exp(x)-1)) + # Convert log-scale to original
  labs(x = expression("Area (km"^2*")"), y = "Networks") +
  theme_classic()

#research invs by networks
res_inv <- ggplot(webs_country,
                  aes(x = log_PropGDP_median, y = log_Total_webs_by_country, color = Continent)) +
  geom_point() +
  scale_color_viridis(discrete = T) +
  geom_smooth(method = "glm",
              method.args = list(family = "quasipoisson"), # Closest match to neg. binomial in ggplot
              formula = y ~ scale(x), alpha = 0.15,
              se = TRUE) +
  scale_x_continuous(labels = function(x) round(exp(x)-1, 2)) +
  scale_y_continuous(labels = function(x) round(exp(x)-1)) + # Convert log-scale to original
  labs(x = "Research Investment", y = "") +
  theme_classic()

#Bee species
bees <- ggplot(webs_country,
               aes(x = log_CL_Species, y = log_Total_webs_by_country, color = Continent)) +
  geom_point(show.legend = FALSE) +
  scale_color_viridis(discrete = T) +
  geom_smooth(method = "glm",
              method.args = list(family = "quasipoisson"), # Closest match to neg. binomial in ggplot
              formula = y ~ scale(x), alpha = 0.15,
              se = TRUE, show.legend = FALSE) +
  scale_x_continuous(labels = function(x) round(exp(x)-1, -1)) + # Convert log-scale to original
  scale_y_continuous(labels = function(x) round(exp(x)-1, -1)) + 
  labs(x = "Bee Species Richness", y = "") +
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
## ***********************************************
webs_reuse <- webs_complete %>%
  filter(!is.na(years_since_pub),
         years_since_pub<90)

reuse <- ggplot(webs_reuse,
                aes(x = years_since_pub, y = webs_reuse_count, color = Continent)) +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  geom_smooth(
    method = "glm",
    method.args = list(family = "quasipoisson"),
    formula = y ~ scale(x),
    alpha = 0.15,
    se = TRUE,
    show.legend = FALSE
  ) +
  labs(
    x     = "Years Since Publication",
    y     = "Network Reuse",
    color = "Continent",
    fill  = "Continent"
  ) +
  theme_classic() +
  theme(
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.title       = element_text(hjust = 0.5)
  ) +
  guides(color = guide_legend(nrow = 1))

reuse

## ***********************************************
webs_reuse$Status <- ifelse(webs_reuse$webs_reuse_count > 0, "Reused", "Original")

yearly_counts <- webs_reuse %>%
  group_by(Publi_Year, Continent, Status) %>%
  summarise(count = n(), .groups = "drop")

ggplot(yearly_counts, aes(x = Publi_Year, y = count, color = Status)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("Original" = "#1b9e77", "Reused" = "#d95f02")) +  # Match colors
  labs(
    x = "Year",
    y = "Number of networks published",
    color = "Status"
  ) +
  facet_wrap(~ Continent) +
  theme_bw() +  # use a base theme with axes
  theme(
    legend.position = "right",
    text = element_text(size = 14),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank(),  # remove gridlines
    panel.border = element_rect(color = "black", fill = NA)  # adds box with axes to every panel
  )

## ***********************************************
