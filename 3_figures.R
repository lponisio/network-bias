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
library(patchwork)
#data
webs <- read.csv("network-bias-saved/saved/webs_complete.csv")

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
webs_complete <- webs[webs$Continent != "Oceania",]

webs_complete$Continent <- factor(webs_complete$Continent,
                                  levels=c("North America",
                                           "South America",
                                           "Africa",
                                           "Europe",
                                           "Asia"))


webs_country <- webs_complete %>%
  distinct(adm0_a3, .keep_all = TRUE) %>%
  filter(!is.na(Continent) & 
           !is.na(AREA) & 
           !is.na(PropGDP_median) & 
           !is.na(CL_Species))


# Ensure the dataset contains the required transformed variables
webs_country$log_AREA <- log(webs_country$AREA+1)
webs_country$log_PropGDP_median <- log(webs_country$PropGDP_median+1)
webs_country$log_CL_Species <- log(webs_country$CL_Species+1)
webs_country$log_Total_webs_by_country <- log(webs_country$Total_webs_by_country+1)
webs_country$log_ResInvs_Density <- log(webs_country$ResInvs_Density+1)

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
                  aes(x = log_ResInvs_Density, y = log_Total_webs_by_country, color = Continent)) +
  geom_point() +
  scale_color_viridis(discrete = T) +
  geom_smooth(method = "glm",
              method.args = list(family = "quasipoisson"), # Closest match to neg. binomial in ggplot
              formula = y ~ scale(x), alpha = 0.15,
              se = TRUE) +
  scale_x_continuous(labels = function(x) round(exp(x)-1, 2)) +
  scale_y_continuous(labels = function(x) round(exp(x)-1)) + # Convert log-scale to original
  labs(x = expression("Research investment density (USD/"* km^2 *")"), y = "") +
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
  labs(x = expression("Bee species richness"), y = "") +
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
  filter(!is.na(webs_reuse_count))%>%
  group_by(Publi_Year, Continent, Status) %>%
  summarise(count = n(), .groups = "drop")

reuse <- ggplot(yearly_counts, aes(x = Publi_Year, y = count, color = Status)) +
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
ggsave(reuse, file = paste0(savefilepath, "/reuse.png"), width = 15, height = 10, dpi = 300)


## ***********************************************
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

#webs[webs$Country == "Greenland",]$ISO3 <- "GRL"

# Summarize number of networks per country
country_summary <- webs %>%
  distinct(adm0_a3, Total_webs_by_country) %>%
  rename(value=Total_webs_by_country)


# Load world map as an sf object
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge your ISO3 summary data to the world map
world_with_data <- world %>%
  left_join(country_summary, by = "adm0_a3")

world_with_data[is.na(world_with_data$value),]$adm0_a3

coor <- webs %>%
  filter(!is.na(LAT.x),
         !is.na(LAT.y),
         !is.na(webs_reuse_count)) %>%
  rename(lat = LAT.x, lon = LONG, Use_Frequency = webs_reuse_count) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)  # convert to sf points

# First, make sure to assign a new column for custom fill
world_with_data$fill_value <- ifelse(is.na(world_with_data$value), NA,
                                     ifelse(world_with_data$value == 0, -1, world_with_data$value))

map_net <- ggplot() +
  # Base world map with custom fill
  geom_sf(data = world_with_data, aes(fill = fill_value), color = "white", size = 0.1, na.rm = FALSE) +
  
  # Points
  geom_sf(data = coor, color = "black", size = 1.6) +
  geom_sf(data = coor, aes(color = Use_Frequency), size = 1.5) +
  
  # Custom fill scale
  scale_fill_gradientn(
    colours = c("grey", "#EDF8E9", "#5AAE61", "#1B7837"),
    values = scales::rescale(c(-1, 1, max(world_with_data$fill_value, na.rm = TRUE))),
    name = "Number of networks",
    na.value = "black",
    guide = guide_colourbar(
      direction = "horizontal",
      barheight = unit(2, "mm"),
      barwidth = unit(50, "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 0.5
    )
  )+
  
  # Point color scale
  scale_color_gradientn(
    colours = c("#FFE5B4", "#FDB863", "#E08214", "#B35806"),
    name = "Network use frequency",
    guide = guide_colourbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 0.5
    )
  ) +
  
  # Coordinate limits
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
  
  labs(x = "Longitude", y = "Latitude") +
  
  theme_classic() +
  theme(
    legend.box = "vertical",
    
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(1, 100, 1, 1)
  )

map_net
ggsave(map_net, file = paste0(savefilepath, "/map_net.png"), width = 15, height = 6, dpi = 500)


#-----------------------------------------------
#just Europe
map_europe <- ggplot() +
  geom_sf(data = world_with_data, aes(fill = fill_value), color = "white", size = 0.1) +
  geom_sf(data = coor, color = "black", size = 2.5) +
  geom_sf(data = coor, aes(color = Use_Frequency), size = 2) +
  scale_fill_gradientn(
    colours = c("grey", "#EDF8E9", "#5AAE61", "#1B7837"),
    values = scales::rescale(c(-1, 1, max(world_with_data$fill_value, na.rm = TRUE))),
    na.value = "black")+
    scale_color_gradientn(colours = c("#FFE5B4", "#FDB863", "#E08214", "#B35806")) +
  coord_sf(xlim = c(-15, 40), ylim = c(35, 70), expand = FALSE) +  # <-- zoomed-in coords
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 16, face = "bold"),
    #legend.text = element_text(size = 10),
    #legend.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(map_europe, file = paste0(savefilepath, "/map_net_europe.png"), width = 6, height = 6, dpi = 300)




#-----------------------------------------------
# Load datasets
#-----------------------------------------------
#webs <- read.csv("network-bias-saved/raw/webs.csv", sep = ";")
#bees <- read.csv("network-bias-saved/raw/bees_by_country.csv", sep = ",")
#gdp  <- read.csv("network-bias-saved/raw/gdp.csv", sep = ",")

# Summarize number of networks per country
country_summary <- webs %>%
  distinct(adm0_a3, Total_webs_by_country, CL_Species, ResInvs_Density, ResInvestTotal) %>%
  filter(!is.na(Total_webs_by_country) & 
           !is.na(ResInvs_Density) & 
           !is.na(CL_Species))  %>%
  rename(value=Total_webs_by_country)
#zeros don't work with the distortion
country_summary$value <-country_summary$value+1


#-----------------------------------------------
# Prepare base world map (excluding Antarctica)
#-----------------------------------------------

world_map <- ne_countries(returnclass = "sf") %>%
  dplyr::select(adm0_a3) %>%
  st_transform(crs = "+proj=robin")
#-----------------------------------------------
# 1. Cartogram based on number of networks per country
#-----------------------------------------------

world_data1 <- left_join(world_map, country_summary, by = c("adm0_a3"))

#world_data1[is.na(world_data1$value), "value"] <- 1  # Fill NAs with 1 to avoid zero-size countries

world_carto1 <- cartogram_cont(world_data1[!is.na(world_data1$value),], "value", maxSizeError = .001)
plot(world_carto1["value"])  # Plot cartogram with network count

#-----------------------------------------------
# 2. Cartogram based on number of bee species per country
#-----------------------------------------------

world_carto2 <- cartogram_cont(world_data1[!is.na(world_data1$CL_Species),], "CL_Species", maxSizeError = .001)
plot(world_carto2["CL_Species"])

#-----------------------------------------------
# 3. Cartogram based on Proportional GDP 
#-----------------------------------------------
world_data1$ResInvestTotal_log <-log(world_data1$ResInvestTotal+1)

world_carto3 <- cartogram_cont(world_data1[!is.na(world_data1$ResInvestTotal_log),] , "ResInvestTotal_log", maxSizeError = .0001)
plot(world_carto3["ResInvestTotal_log"])

#-----------------------------------------------
# 4. Plot cartogram with ggplot2
#-----------------------------------------------

# Custom color palette (orange to green)
custom_palette <- c("#FFE5B4", "#FDB863", "#E08214", "#5AAE61", "#1B7837")


# Shared guide settings
legend_guide <- guide_colourbar(
  direction = "horizontal",
  title.position = "top",
  title.hjust = 0.5,
  label.hjust = 0.5,
  barwidth = unit(50, "mm"),
  barheight = unit(3, "mm")
)

# 1. Number of networks
p1 <- ggplot(world_carto1) +
  geom_sf(aes(fill = value), color = "gray20", linewidth = 0.2) +
  scale_fill_gradientn(colors = custom_palette, name = "Number of networks", guide = legend_guide) +
  labs(x = "Longitude", y = NULL) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )


# 2. Research investment
p2 <- ggplot(world_carto3) +
  geom_sf(aes(fill = ResInvestTotal_log), color = "gray20", linewidth = 0.2) +
  scale_fill_gradientn(colors = custom_palette, name = "Total research investment (log)", , guide = legend_guide,
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
  scale_fill_gradientn(colors = custom_palette, name = "Number of bee species", guide = legend_guide) +
  labs(x = "Longitude", y = NULL) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )


# Combine the three plots vertically
final_plot <- p1 / p2 / p3 

ggsave(final_plot, file = paste0(savefilepath, "/combined_cartogram_maps.png"), width = 10, height = 15, dpi = 300)



