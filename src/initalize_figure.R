webs <- read.csv("../network-bias-saved/saved/webs_complete.csv")
source("../network-bias/2_models.R")
savefilepath <- c("../network-bias-saved/manuscript/figures")

# =========================================================
# Prediction Data Generators & Inverse-Standardization Helpers
#
# Purpose:
# - Create continent-wise prediction grids for plotting GLM/GLMM effects,
#   varying one standardized log predictor at a time while holding others
#   at their means; return fitted values and 95% Wald CIs on the response scale.
# - Provide helpers to back-transform standardized log variables to raw units
#   (for axis ticks and readable labels).
# =========================================================

# For >2 years dataset

## Build prediction grid across continents, varying one focal predictor (`xvar`)
generate_prediction_data <- function(model, data, continents, xvar,
                                     log_AREA_mean, log_SR_mean,
                                     n_points = 100) {
  
  ## helper: safe numeric extraction 
  num <- function(v) as.numeric(v)
  
  ## precompute means (on numeric)
  mean_ResInvs2 <- mean(num(data$log_ResInvs_Density_2), na.rm = TRUE)
  mean_SRdens   <- mean(num(data$log_CL_Species_density), na.rm = TRUE)  
  mean_AREA     <- mean(num(data$log_AREA), na.rm = TRUE)               
  
  do.call(rbind, lapply(continents, function(ct) {
    
    x_seq <- seq(
      min(num(data[[xvar]]), na.rm = TRUE),
      max(num(data[[xvar]]), na.rm = TRUE),
      length.out = n_points
    )
    
    newdata <- data.frame(
      Continent = ct,
      
      ## columns from the model
      log_ResInvs_Density_2   = if (xvar == "log_ResInvs_Density_2") x_seq else mean_ResInvs2,
      log_CL_Species_density  = if (xvar == "log_CL_Species_density") x_seq else log_SR_mean,
      log_AREA                = if (xvar == "log_AREA") x_seq else log_AREA_mean
    )
    
    pred <- predict(model, newdata = newdata, type = "link", se.fit = TRUE)
    
    newdata[[xvar]] <- x_seq
    newdata$fit <- exp(pred$fit)
    newdata$lwr <- exp(pred$fit - 1.96 * pred$se.fit)
    newdata$upr <- exp(pred$fit + 1.96 * pred$se.fit)
    
    newdata
  }))
}


# Convert raw ticks (no log, no standardization) -> standardized-log scale used in xvar
raw_to_stdlog <- function(raw_ticks, x_dw) {
  mu <- attr(x_dw, "center")
  sig <- attr(x_dw, "scale")
  (log(raw_ticks) - mu) / sig
}

# Nice scientific labels for raw ticks
raw_sci_labels <- function(raw_ticks, digits = 2) {
  scales::label_scientific(digits = digits)(raw_ticks)
}

# For >5 years dataset

## Build prediction grid across continents, varying one focal predictor (`xvar`)
generate_prediction_data_5y <- function(model, data, continents, xvar,
                                        log_AREA_mean, log_SR_mean,
                                        n_points = 100) {
  
  num <- function(v) as.numeric(v)
  
  mean_ResInvs5 <- mean(num(data$log_ResInvs_Density_5), na.rm = TRUE)
  
  do.call(rbind, lapply(continents, function(ct) {
    
    x_seq <- seq(
      min(num(data[[xvar]]), na.rm = TRUE),
      max(num(data[[xvar]]), na.rm = TRUE),
      length.out = n_points
    )
    
    newdata <- data.frame(
      Continent = ct,
      
      log_ResInvs_Density_5  = if (xvar == "log_ResInvs_Density_5") x_seq else mean_ResInvs5,
      log_CL_Species_density = if (xvar == "log_CL_Species_density") x_seq else log_SR_mean,
      log_AREA               = if (xvar == "log_AREA") x_seq else log_AREA_mean
    )
    
    pred <- predict(model, newdata = newdata, type = "link", se.fit = TRUE)
    
    newdata[[xvar]] <- x_seq
    newdata$fit <- exp(pred$fit)
    newdata$lwr <- exp(pred$fit - 1.96 * pred$se.fit)
    newdata$upr <- exp(pred$fit + 1.96 * pred$se.fit)
    
    newdata
  }))
}



# Convert raw ticks (no log, no standardization) -> standardized-log scale used in xvar
raw_to_stdlog <- function(raw_ticks, x_dw) {
  mu <- attr(x_dw, "center")
  sig <- attr(x_dw, "scale")
  (log(raw_ticks) - mu) / sig
}

# Nice scientific labels for raw ticks
raw_sci_labels <- function(raw_ticks, digits = 2) {
  scales::label_scientific(digits = digits)(raw_ticks)
}

