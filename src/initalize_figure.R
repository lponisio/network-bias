
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
generate_prediction_data <- function(model, data, continents, xvar, log_AREA_mean, log_SR_mean, n_points = 100) {
  do.call(rbind, lapply(continents, function(ct) {
    x_seq <- seq(min(data[[xvar]], na.rm = TRUE),
                 max(data[[xvar]], na.rm = TRUE),
                 length.out = n_points)
    
    newdata <- data.frame(
      Continent = ct,
      log_ResInvs_Density = if (xvar == "log_ResInvs_Density") x_seq else mean(data$log_ResInvs_Density, na.rm = TRUE),
      log_CL_Species_density = if (xvar == "log_CL_Species_density") x_seq else log_SR_mean,
      log_AREA = if (xvar == "log_AREA") x_seq else log_AREA_mean
    )
    
    pred <- predict(model, newdata = newdata, type = "link", se.fit = TRUE)
    newdata[[xvar]] <- x_seq
    newdata$fit <- exp(pred$fit)
    newdata$lwr <- exp(pred$fit - 1.96 * pred$se.fit)
    newdata$upr <- exp(pred$fit + 1.96 * pred$se.fit)
    
    return(newdata)
  }))
}
inv_standardize_2 <- function(x, mean, sd) {
  raw_log <- x * sd + mean
  raw_val <- exp(raw_log)
  round(raw_val, -2)
}

inv_standardize_label <- function(x, mean, sd) {
  # Undo standardization and log transformation
  raw_log <- x * sd + mean
  raw_val <- exp(raw_log)
  
  # Format as scientific notation with 2 significant digits
  label_func <- scales::label_scientific(digits = 2)
  label_func(raw_val)
}
