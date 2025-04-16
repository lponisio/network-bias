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
library(ggplot2)
library(tidyverse)
library(maps)
library(grid) # For unit()

# Load required libraries
library(rnaturalearth)  # for world map data
library(sf)             # for spatial data manipulation
library(cartogram)      # for generating cartograms
library(dplyr)
library(ggplot2)
library(scales)

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


# Load the dataset
webs <- read.csv("network-bias-saved/raw/webs.csv", sep = ";")

# Summarize number of networks per country
country_summary <- webs %>%
  group_by(Country) %>%
  summarise(value = n())

# Prepare point data with coordinates
coor <- webs %>%
  filter(!is.na(LAT) & !is.na(LONG)) %>%
  rename(lat = LAT, lon = LONG)

# Load world map
world <- map_data("world")

#Plot map
map_net <- ggplot() +
  # Base map
  geom_map(
    data = world, map = world,
    aes(map_id = region),
    color = "black", fill = "#DDDDDD", size = 0.1
  ) +
  # Country fill (green)
  geom_map(
    data = country_summary, map = world,
    aes(map_id = Country, fill = value),
    color = "white", size = 0.25
  ) +
  # Network points (orange)
  geom_point(
    data = coor,
    aes(x = lon, y = lat, color = Use_Frequency),
    alpha = 0.9, size = 3
  ) +
  # Fill scale (green)
  scale_fill_gradientn(
    colours = c("#D9F0D3", "#A6DBA0", "#5AAE61", "#1B7837"),
    name = "Number of networks",
    guide = guide_colourbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),  # smaller legend height
      barwidth = unit(50, units = "mm"),  # smaller legend width
      draw.ulim = FALSE,
      title.hjust = 0.5,
      label.hjust = 0.5,
      title.position = "top"
    )
  ) +
  # Point color scale (orange)
  scale_color_gradientn(
    colours = c("#FFE5B4", "#FDB863", "#E08214", "#B35806"),
    name = "Network use frequency",
    guide = guide_colourbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = FALSE,
      title.hjust = 0.5,
      label.hjust = 0.5,
      title.position = "top"
    )
  ) +
  # Fix the map area to fully include South America (Argentina)
  coord_fixed(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
  
  # Axis labels
  labs(x = "Longitude", y = "Latitude") +
  
  # Theme settings
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(5, 5, 5, 5)  # reduce white space around the plot
  )

#Saving
tiff('nets_map.tif', w=6000, h=3400, units="px", res=600, compression = "lzw")
map_net
dev.off()
## ***********************************************


#-----------------------------------------------
# Load datasets
#-----------------------------------------------
webs <- read.csv("network-bias-saved/raw/webs.csv", sep = ";")
bees <- read.csv("network-bias-saved/raw/bees_by_country.csv", sep = ",")
gdp  <- read.csv("network-bias-saved/raw/gdp.csv", sep = ",")

#-----------------------------------------------
# Prepare base world map (excluding Antarctica)
#-----------------------------------------------
world_map <- ne_countries(returnclass = "sf") %>%
  select(gu_a3) %>%
  filter(gu_a3 != "ATA") %>%
  st_transform(crs = "+proj=robin")  # Robinson projection

#-----------------------------------------------
# 1. Cartogram based on number of networks per country
#-----------------------------------------------
net.country <- webs %>%
  count(ISO3)

world_data1 <- left_join(world_map, net.country, by = c("gu_a3" = "ISO3"))
world_data1[is.na(world_data1$n), "n"] <- 1  # Fill NAs with 1 to avoid zero-size countries

world_carto1 <- cartogram_cont(world_data1, "n", maxSizeError = 1.5)
plot(world_carto1["n"])  # Plot cartogram with network count

#-----------------------------------------------
# 2. Cartogram based on number of bee species per country
#-----------------------------------------------
# Rename columns for easier use
bees2 <- bees %>% select(ISO3, CL_Species)

world_data2 <- left_join(world_map, bees2, by = c("gu_a3" = "ISO3")) %>%
  na.omit()

world_carto2 <- cartogram_cont(world_data2, "CL_Species", maxSizeError = 1.5)
plot(world_carto2["CL_Species"])

#-----------------------------------------------
# 3. Cartogram based on GDP in 2020
#-----------------------------------------------
real.gdp <- gdp %>%
  select(Country.Code, X2020)

world_data3 <- left_join(world_map, real.gdp, by = c("gu_a3" = "Country.Code")) %>%
  na.omit()

world_carto3 <- cartogram_cont(world_data3, "X2020", maxSizeError = 1.5)
plot(world_carto3["X2020"])

#-----------------------------------------------
# 4. Plot cartogram with ggplot2
#-----------------------------------------------

# Custom color palette (orange to green)
custom_palette <- c("#FFE5B4", "#FDB863", "#E08214", "#5AAE61", "#1B7837")

# 1. Number of networks
p1 <- ggplot(world_carto1) +
  geom_sf(aes(fill = n), color = "gray20", linewidth = 0.2) +
  scale_fill_gradientn(colors = custom_palette, name = "Number of networks") +
  labs(x = "Longitude", y = NULL) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

# 2. Research investment
p2 <- ggplot(world_carto3) +
  geom_sf(aes(fill = X2020), color = "gray20", linewidth = 0.2) +
  scale_fill_gradientn(colors = custom_palette, name = "Research investment\n($ US Dollar)",
                       labels = label_number(scale_cut = cut_short_scale())) +
  labs(x = "Longitude", y = NULL) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

# 3. Number of bee species
p3 <- ggplot(world_carto2) +
  geom_sf(aes(fill = CL_Species), color = "gray20", linewidth = 0.2) +
  scale_fill_gradientn(colors = custom_palette, name = "Number of bee species") +
  labs(x = "Longitude", y = NULL) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )






