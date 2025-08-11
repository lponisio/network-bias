webs <- read.csv("network-bias-saved/saved/webs_complete.csv")
source("network-bias/2_models.R")


savefilepath <- c("network-bias-saved/manuscript/figures")

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

## Build prediction grid across continents, varying one focal predictor (`xvar`)
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
    
    pred <- predict(model, newdata = newdata, type = "link", se.fit = TRUE)  # predict on link scale
    newdata[[xvar]] <- x_seq
    newdata$fit <- exp(pred$fit)                                             # back-transform to response scale
    newdata$lwr <- exp(pred$fit - 1.96 * pred$se.fit)                        # 95% Wald CI
    newdata$upr <- exp(pred$fit + 1.96 * pred$se.fit)
    
    return(newdata)
  }))
}

## Inverse of z-standardize(log(x)): x_raw = exp(x_std * sd + mean)
inv_standardize_2 <- function(x, mean, sd) {
  raw_log <- x * sd + mean
  raw_val <- exp(raw_log)
  round(raw_val, 0)
}

## Inverse transform for axis labels (standardized log → raw), formatted in scientific notation
inv_standardize_label <- function(x, mean, sd) {
  # Undo standardization and log transformation
  raw_log <- x * sd + mean
  raw_val <- exp(raw_log)
  
  # Format as scientific notation with 2 significant digits
  label_func <- scales::label_scientific(digits = 2)
  label_func(raw_val)
}


