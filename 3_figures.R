rm(list=ls())

setwd("network-bias")
source("src/initalize_packages.R")
source("src/initalize_figure.R")

# =========================================================
# Country-level Variables — Predictions & Plot
# =========================================================

# Unique continent levels used for prediction lines
continents <- unique(webs_country$Continent)

# -------------------------------------------------------
# Research Investment
# -------------------------------------------------------
# Build prediction grid varying log_ResInvs_Density across continents;
# other predictors are held at their (standardized-log) means
prediction_data <- generate_prediction_data(
  model = network_use,
  data = webs_country,
  continents = continents,
  xvar = "log_ResInvs_Density",
  log_AREA_mean = mean(webs_country$log_AREA, na.rm = TRUE),
  log_SR_mean = mean(webs_country$log_CL_Species_density, na.rm = TRUE)
)

# Compute mean and SD of the raw log(R&D density) for inverse-standardization of ticks
log_resinv_raw <- log(webs_country$ResInvs_Density)
log_resinv_mean <- mean(log_resinv_raw, na.rm = TRUE)
log_resinv_sd <- sd(log_resinv_raw, na.rm = TRUE)

# Pretty tick marks to show on the raw (unlogged) scale of R&D per km²
pretty_vals <- c(10, 1000, 10000, 100000,1000000,10000000, 100000000, 1000000000, 10000000000)

# Convert pretty raw values back to standardized-log scale used on x-axis
standardized_vals <- (log(pretty_vals) - log_resinv_mean) / log_resinv_sd

# Plot observed data and model predictions (with 95% ribbons) by continent
ResInvs <- ggplot(webs_country,
                  aes(x = log_ResInvs_Density, y = Total_webs_by_country, color = Continent)) +
  geom_point(show.legend = FALSE, size = 2) +                              # observed points
  geom_line(data = prediction_data, show.legend = FALSE,                   # predicted mean
            aes(x = log_ResInvs_Density, y = fit, color = Continent), 
            size = 1) +
  geom_ribbon(data = prediction_data, show.legend = FALSE,                 # 95% CI ribbon
              aes(x = log_ResInvs_Density, ymin = lwr, ymax = upr, fill = Continent), 
              alpha = 0.1, inherit.aes = FALSE) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_x_continuous(                                                       # show raw-scale ticks
    breaks = standardized_vals,
    labels = pretty_vals
  )+
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = expression(bold("Research investment per km"^2*"")), 
       y = "") +
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),       # axis titles (x and y)
    axis.text = element_text(size = 14),        # axis tick labels
    plot.title = element_text(size = 18, face = "bold")  # to add title later
  ) 

# Display plot
ResInvs

# -------------------------------------------------------
# Bee species
# -------------------------------------------------------

# Generate prediction grid for bee species density across continents
prediction_data_sr <- generate_prediction_data(
  model = network_use,
  data = webs_country,
  continents = continents,
  xvar = "log_CL_Species_density",
  log_AREA_mean = mean(webs_country$log_AREA, na.rm = TRUE),
  log_SR_mean = mean(webs_country$log_CL_Species_density, na.rm = TRUE))

# Get mean and SD of the unstandardized log species richness density
log_sr_raw <- log(webs_country$CL_Species_Density)
log_sr_mean <- mean(log_sr_raw, na.rm = TRUE)
log_sr_sd <- sd(log_sr_raw, na.rm = TRUE)

# Choose readable raw-scale tick marks for species per km²
pretty_vals <- c(.000001,.00001,.0001,.001,.01, 0.15 )

# Convert raw-scale ticks to standardized-log scale used on x-axis
standardized_vals <- (log(pretty_vals) - log_sr_mean) / log_sr_sd

# Plot observed data and model predictions (with 95% ribbons) by continent
sr_plot <- ggplot(webs_country,
                  aes(x = log_CL_Species_density, y = Total_webs_by_country, color = Continent)) +
  geom_point(show.legend = FALSE, size = 2) +
  geom_line(data = prediction_data_sr, show.legend = FALSE,
            aes(x = log_CL_Species_density, y = fit, color = Continent), 
            size = 1) +
  geom_ribbon(data = prediction_data_sr, show.legend = FALSE,
              aes(x = log_CL_Species_density, ymin = lwr, ymax = upr, fill = Continent), 
              alpha = 0.1, inherit.aes = FALSE) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_x_continuous(
    breaks = standardized_vals,
    labels = pretty_vals
  )+
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = expression(bold("Bee species per km"^2*" (log)")), 
       y = "Number of Networks") +theme_classic()+
  theme(
    axis.title = element_text(size = 16),       # axis titles (x and y)
    axis.text = element_text(size = 14),        # axis tick labels
    plot.title = element_text(size = 18, face = "bold")  # if you add title later
  ) 

# Display plot
sr_plot

# -------------------------------------------------------
# Area (km²)
# -------------------------------------------------------

# Build prediction grid for AREA across continents
prediction_data_area <- generate_prediction_data(
  model = network_use,
  data = webs_country,
  continents = continents,
  xvar = "log_AREA",
  log_AREA_mean = mean(webs_country$log_AREA, na.rm = TRUE),
  log_SR_mean = mean(webs_country$log_CL_Species_density, na.rm = TRUE))

# Get mean and SD of unstandardized log(AREA) for tick back-transformation
log_area_raw <- log(webs_country$AREA)
log_area_mean <- mean(log_area_raw, na.rm = TRUE)
log_area_sd <- sd(log_area_raw, na.rm = TRUE)

# Raw-scale tick values (km²) to display on x-axis
pretty_vals <- c(10, 100, 1000, 10000, 100000,1000000,10000000, 100000000, 1000000000, 10000000000)

# Convert raw-scale ticks to standardized-log scale used on x-axis
standardized_vals <- (log(pretty_vals) - log_area_mean) / log_area_sd

# Plot observed data and model predictions (with 95% ribbons) by continent
area_plot <- ggplot(webs_country,
                    aes(x = log_AREA, y = Total_webs_by_country, color = Continent)) +
  geom_point(show.legend = FALSE, size = 2) +
  geom_line(data = prediction_data_area, show.legend = FALSE,
            aes(x = log_AREA, y = fit, color = Continent), 
            size = 1) +
  geom_ribbon(data = prediction_data_area, show.legend = FALSE,
              aes(x = log_AREA, ymin = lwr, ymax = upr, fill = Continent), 
              alpha = 0.1, inherit.aes = FALSE) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_x_continuous(
    breaks = standardized_vals,
    labels = pretty_vals
  )+
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = expression(bold("Area (km"^2*")")), 
       y = "Number of Networks") +
  theme_classic()  +
  theme(
    axis.title = element_text(size = 16),       # axis titles (x and y)
    axis.text = element_text(size = 14),        # axis tick labels
    plot.title = element_text(size = 18, face = "bold")  # if you add title later
  ) +
  guides(color = guide_legend(nrow = 1)) 

# Display plot
area_plot

# Plot (with legend on the right)
area_plot_with_legend <- ggplot(webs_country,
                                aes(x = log_AREA, y = Total_webs_by_country, color = Continent)) +
  geom_point(show.legend = FALSE, size = 2) +
  geom_line(data = prediction_data_area, 
            aes(x = log_AREA, y = fit, color = Continent), 
            size = 1) +
  geom_ribbon(data = prediction_data_area, 
              aes(x = log_AREA, ymin = lwr, ymax = upr, fill = Continent), 
              alpha = 0.1, inherit.aes = FALSE) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_x_continuous(
    breaks = standardized_vals,
    labels = pretty_vals
  )+
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = expression(bold("Area (km"^2*")")), 
       y = "") +
  theme_classic(base_size = 10)  +
  theme(
    axis.title = element_text(size = 16),       # axis titles (x and y)
    axis.text = element_text(size = 14),        # axis tick labels
    legend.title = element_text(size = 18),     # legend title
    legend.text = element_text(size = 16),      # legend labels
    plot.title = element_text(size = 18, face = "bold")  # if you add title later
  ) +  theme(legend.position = "right")

# Display plot
area_plot_with_legend

# =========================================================
# Assemble Multi-panel Figure (Patchwork) & Export
# =========================================================

# Extract the legend as a ggplot object
legend_obj <- get_legend(area_plot_with_legend)

# Turn it into a plot so it can go in patchwork layout
legend_plot <- ggpubr::as_ggplot(legend_obj)
legend_plot

# Plot combined plots: Bee species, Research investment and Area (km²)
combined_plot <- (sr_plot + theme(legend.position = "none", plot.tag.position = c(0, 0.98)) + ggtitle("A") | 
                    ResInvs + theme(legend.position = "none", plot.tag.position = c(0, 0.98)) + ggtitle("B")) / 
  (area_plot + theme(legend.position = "none", plot.tag.position = c(0, 0.98)) + ggtitle("C") | 
     legend_plot + theme(legend.position = "none")) +
  plot_layout(guides = 'collect')
combined_plot

# Export figure
ggsave(combined_plot, file = paste0(savefilepath, "/Picture2.jpg"), height = 10, width = 12)

# =========================================================
# Network Re-use — Marginal Predictions over Time Since Publication
# =========================================================

# Get the mean and standard deviation of the original years_since_pub
# Compute mean and sd for inverse transform later
mean_years_since_pub <- mean(webs_reuse$years_since_pub, na.rm = TRUE)
sd_years_since_pub <- sd(webs_reuse$years_since_pub, na.rm = TRUE)

# Create prediction data frame for each continent over a range of log-transformed years
new_data <- expand.grid(
  log_years_since_pub = seq(min(webs_reuse$log_years_since_pub), max(webs_reuse$log_years_since_pub), length.out = 100),
  Continent = unique(webs_reuse$Continent)
)

# Predict without random effects (marginal/predicting average trend)
preds <- predict(reuse_mod, newdata = new_data, re.form = NA, se.fit = TRUE)

# Add predictions and back-transform x-axis
new_data <- new_data %>%
  mutate(
    fit = exp(preds$fit),  # log_pub_count → pub_count
    lwr = exp(preds$fit - 1.96 * preds$se.fit),
    upr = exp(preds$fit + 1.96 * preds$se.fit),
    years_since_pub = log_years_since_pub * sd_years_since_pub + mean_years_since_pub
  )

# Plot observed reuse counts and continent-specific marginal predictions over time
reuse <- ggplot() +
  geom_point(data = webs_reuse, aes(x = years_since_pub, y = pub_count, color = Continent), alpha = 0.6) +
  geom_line(data = new_data, aes(x = years_since_pub, y = fit, color = Continent), size = 1) +
  geom_ribbon(data = new_data, aes(x = years_since_pub, ymin = lwr, ymax = upr, fill = Continent),
              alpha = 0.2, color = NA) +
  coord_cartesian(ylim = c(0, 50)) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_classic(base_size = 14) +
  labs(x = "Years since publication", y = "Number of Reuses") +
  theme(legend.title = element_blank())

# Display plot
reuse

#Export
ggsave(reuse, file = paste0(savefilepath, "/Picture5.png"), height = 5, width = 12)

# =========================================================
# Global Map — Network Counts per Country & Reuse Frequency
# =========================================================

# Summarize number of networks per country (rename for mapping join)
country_summary <- webs_country %>%
  rename(value=Total_webs_by_country)

# Load world map as an sf object (Natural Earth polygons)
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge ISO3 summary data to the world map (key: adm0_a3)
world_with_data <- world %>%
  left_join(country_summary, by = "adm0_a3")

# Prepare point layer: reuse coordinates as sf points (WGS84)
coor <- webs_reuse %>%
  filter(!is.na(LAT),
         !is.na(LONG),
         !is.na(pub_count)) %>%
  rename(lat = LAT, lon = LONG, Use_Frequency = pub_count) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)  # convert to sf points

# Mark true zeros distinctly: NA stays NA; 0 becomes -1 for a separate color bin
world_with_data$fill_value <- ifelse(is.na(world_with_data$value), NA,
                                     ifelse(world_with_data$value == 0, -1, world_with_data$value))

# Plot map
map_net <- ggplot() +
  # Base world map with custom fill (countries)
  geom_sf(data = world_with_data, aes(fill = fill_value), color = "white", size = 0.1, na.rm = FALSE) +
  
  # Points: outline for contrast + color scale for reuse frequency
  geom_sf(data = coor, color = "black", size = 2) +
  geom_sf(data = coor, aes(color = Use_Frequency), size = 1.5) +
  
  # Continuous fill scale for country counts (grey for zeros, green gradient for >0)
  scale_fill_gradientn(
    colours = c("grey", "#EDF8E9", "#5AAE61", "#1B7837"),
    values = scales::rescale(c(-1, 1, max(world_with_data$fill_value, na.rm = TRUE))),
    name = "Number of networks",
    na.value = "#ededed",
    guide = guide_colourbar(
      direction = "horizontal",
      barheight = unit(2, "mm"),
      barwidth = unit(50, "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 0.5
    )
  )+
  
  # Point color scale for network reuse frequency
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
  
  # Map extent and axes
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  
  # Theme and layout
  theme_classic() +
  theme(
    legend.box = "vertical",
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(1, 100, 1, 1)
  )

# Draw map
map_net

# Export
ggsave(map_net, file = paste0(savefilepath, "/map_net.png"), width = 15, height = 6, dpi = 500)


# =========================================================
# Europe Zoom Map — Network Counts & Reuse Frequency
# =========================================================

#Just Europe due points over concentration
map_europe <- ggplot() +
  # Base map with country fills
  geom_sf(data = world_with_data, aes(fill = fill_value), color = "white", size = 0.1) +
  # Point outlines for contrast
  geom_sf(data = coor, color = "black", size = 2.5) +
  # Points colored by reuse frequency
  geom_sf(data = coor, aes(color = Use_Frequency), size = 2) +
  # Country fill scale (grey for zeros; green gradient for >0)
  scale_fill_gradientn(
    colours = c("grey", "#EDF8E9", "#5AAE61", "#1B7837"),
    values = scales::rescale(c(-1, 1, max(world_with_data$fill_value, na.rm = TRUE))),
    na.value = "#ededed")+
  # Point color scale (reuse frequency)
  scale_color_gradientn(colours = c("#FFE5B4", "#FDB863", "#E08214", "#B35806")) +
  # Zoom to Europe
  coord_sf(xlim = c(-15, 40), ylim = c(35, 70), expand = FALSE) +  # <-- zoomed-in coords
  theme_classic() +
  theme(
    legend.position = "none",                 # hide legend for this zoomed map
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 16, face = "bold"),
    #legend.text = element_text(size = 10),
    #legend.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )

# Display plot
map_europe

# Export
ggsave(map_europe, file = paste0(savefilepath, "/map_net_europe.png"), width = 6, height = 6, dpi = 300)

#-----------------------------------------------
# Cartograms and distort maps
#-----------------------------------------------

# Summarize number of networks per country
country_summary <- webs_country %>%
  rename(value=Total_webs_by_country)

# Zeros don't work with the distortion
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


world_carto1 <- cartogram_cont(world_data1[!is.na(world_data1$value),], "value", maxSizeError = .001)
plot(world_carto1["value"])  # Plot cartogram with network count

#-----------------------------------------------
# 2. Cartogram based on number of bee species per country
#-----------------------------------------------
world_data1$CL_Species_Density_LOG<-log(world_data1$CL_Species_Density +1)

world_carto2 <- cartogram_cont(world_data1[!is.na(world_data1$CL_Species_Density_LOG),], "CL_Species_Density_LOG", maxSizeError = .001)

plot(world_carto2["CL_Species_Density_LOG"])

#-----------------------------------------------
# 3. Cartogram based on Proportional GDP 
#-----------------------------------------------
world_data1$ResInvestTotal_log <-log(world_data1$ResInvs_Density+1)

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
  scale_fill_gradientn(colors = custom_palette, name = "Networks", guide = legend_guide) +
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
  scale_fill_gradientn(colors = custom_palette, name = expression(bold("Research investment per km"^2*" (log)")), guide = legend_guide,
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
  geom_sf(aes(fill = CL_Species_Density_LOG), color = "gray20", linewidth = 0.2) +
  scale_fill_gradientn(colors = custom_palette, name = expression(bold("Bee species per km"^2*" (log)")), guide = legend_guide) +
  labs(x = "Longitude", y = NULL) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

# Combine the three plots with a centered legend
combined_plot <- (p1 / p3 / p2) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 16, face = "bold"))

# Display plot
combined_plot

# Export
ggsave(combined_plot, file = paste0(savefilepath, "/combined_cartogram_maps.png"), width = 10, height = 15, dpi = 300)

