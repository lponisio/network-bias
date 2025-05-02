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
source("network-bias/2_models.R")
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

## ***********************************************



# Load necessary packages
library(dplyr)

# Set color palette for continents
continents <- unique(webs_country$Continent)
continent_colors <- setNames(rainbow(length(continents)), continents)

# Compute means of covariates
log_ResInvs_mean <- mean(webs_country$log_ResInvs_Density, na.rm = TRUE)
log_AREA_mean <- mean(webs_country$log_AREA, na.rm = TRUE)
log_SR_mean <- mean(webs_country$log_CL_Species_density, na.rm = TRUE)


plot_predictor_effect <- function(predictor_var, xlab, main_title, fixed_covariates) {
  # Get z-score range from data
  x_seq <- seq(min(webs_country[[predictor_var]], na.rm = TRUE),
               max(webs_country[[predictor_var]], na.rm = TRUE),
               length.out = 100)
  
  # Get mean and sd of the original log-transformed variable
  original_var <- switch(predictor_var,
                         "log_CL_Species_density" = log(webs_country$CL_Species_Density),
                         "log_ResInvs_Density" = log(webs_country$Research_Investment_Density),
                         "log_AREA" = log(webs_country$Country_Area_km2))
  
  original_mean <- mean(original_var, na.rm = TRUE)
  original_sd <- sd(original_var, na.rm = TRUE)
  
  # For plotting: x values are standardized, but we want axis labels in original units
  plot(webs_country[[predictor_var]], webs_country$Total_webs_by_country,
       col = continent_colors[webs_country$Continent], pch = 16,
       xaxt = "n",  # suppress default x-axis
       xlab = gsub("Log ", "", xlab),
       ylab = "Number of Networks per Country",
       main = main_title)
  
  # Custom x-axis: convert z-scores to real-world (unlogged) scale
  z_ticks <- pretty(range(webs_country[[predictor_var]], na.rm = TRUE), n = 5)
  raw_labels <- exp(z_ticks * original_sd + original_mean)
  
  axis(side = 1, at = z_ticks, labels = round(raw_labels, 1))  # real units
  
  legend("topleft", legend = names(continent_colors),
         col = continent_colors, pch = 16, cex = 0.8)
  
  for (ct in continents) {
    newdata <- data.frame(
      Continent = rep(ct, length(x_seq)),
      log_ResInvs_Density = if (predictor_var == "log_ResInvs_Density") x_seq else rep(fixed_covariates$log_ResInvs_Density, length(x_seq)),
      log_AREA = if (predictor_var == "log_AREA") x_seq else rep(fixed_covariates$log_AREA, length(x_seq)),
      log_CL_Species_density = if (predictor_var == "log_CL_Species_density") x_seq else rep(fixed_covariates$log_CL_Species_density, length(x_seq))
    )
    
    pred <- predict(M1, newdata = newdata, type = "link", se.fit = TRUE)
    
    upr <- pred$fit + 1.96 * pred$se.fit
    lwr <- pred$fit - 1.96 * pred$se.fit
    
    fit_resp <- exp(pred$fit)
    upr_resp <- exp(upr)
    lwr_resp <- exp(lwr)
    
    lines(x_seq, fit_resp, col = continent_colors[ct], lwd = 2)
    
    polygon(c(x_seq, rev(x_seq)),
            c(lwr_resp, rev(upr_resp)),
            col = adjustcolor(continent_colors[ct], alpha.f = 0.2),
            border = NA)
  }
}



# Define fixed covariate means
fixed_covs <- list(
  log_ResInvs_Density = log_ResInvs_mean,
  log_AREA = log_AREA_mean,
  log_CL_Species_density = log_SR_mean
)

# Run plots
plot_predictor_effect("log_CL_Species_density",
                      "Log Bee Species Richness Density",
                      "Effect of Bee Species Richness on Network Count",
                      fixed_covs)

plot_predictor_effect("log_ResInvs_Density",
                      "Log Research Investment Density",
                      "Effect of Research Investment on Network Count",
                      fixed_covs)

plot_predictor_effect("log_AREA",
                      "Log Area",
                      "Effect of Area on Network Count",
                      fixed_covs)



## ***********************************************
## ***********************************************
## ***********************************************
## ***********************************************





# Use ggplot.Predict for plotting the fitted lines
library(rms)

# Set up the model frame
dd <- datadist(webs_country)  # If you have a dataset used to fit the model
options(datadist = "dd")


theta_est <- M1$theta

# Now fit the model using rms::Glm() with the estimated theta
fit <- rms::Glm(Total_webs_by_country ~ Continent+
                  log_ResInvs_Density +
                  log_AREA +
                  log_CL_Species_density, data = webs_country, family = negative.binomial(theta = theta_est))

# Generate predictions
pred <- Predict(fit, log_ResInvs_Density, Continent, fun = exp)

ggplot(pred, aes(x = log_ResInvs_Density, y = yhat, color = Continent, fill = Continent)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(x = "log_ResInvs_Density", y = "Predicted Webs Count") +
  coord_cartesian(ylim = c(0, 80)) +
  theme_minimal()














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



