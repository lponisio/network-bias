rm(list=ls())
## ***********************************************
#workingdirectory
source("~/lab_paths.R")
local.path
setwd(local.path)

source("network-bias/src/initalize_models.R")

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
                                 levels=c("North America",
                                          "South America",
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
webs_country$log_AREA <- scale(log(webs_country$AREA))
webs_country$log_PropGDP_median <- scale(log(webs_country$PropGDP_median))
webs_country$log_CL_Species <- scale(log(webs_country$CL_Species))

M1 <- glm.nb(Total_webs_by_country ~ Continent +
                     log_AREA +
                     log_PropGDP_median +
                     log_CL_Species,
                   #family = nbinom2,
                   data = webs_country)

summary(M1)
check_model(M1)


boot_fun <- function(model, data) {
  # Extract fitted values (mu) and dispersion parameter (theta)
  mu <- fitted(model)  # Mean of the negative binomial distribution
  theta <- model$theta  # Dispersion parameter
  
  # Simulate new response values using the negative binomial distribution
  sim_response <- rnbinom(n = length(mu), size = theta, mu = mu)
  
  # Replace the response column with the simulated data
  data$Total_webs_by_country <- sim_response
  
  # Fit the model to the simulated data
  sim_model <- update(model, data = data)
  
  # Return the coefficients from the simulated model
  return(coef(sim_model))
}


n_iter <- 1000  # Number of bootstrap iterations
boot_results <- replicate(n_iter, boot_fun(M1, webs_country))



boot_results_df <- as.data.frame(t(boot_results))
colnames(boot_results_df) <- names(coef(M1))  # Assign coefficient names
boot_ci <- apply(boot_results_df, 2, function(x) quantile(x, probs = c(0.025, 0.975)))  # 95% CI
boot_ci







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
  filter(!is.na(years_since_pub),
         years_since_pub<90)
webs_reuse$webs_reuse_count


library(MASS)
M1_nb <- glm.nb(webs_reuse_count ~ Continent *years_since_pub, data = webs_reuse)
summary(M1_nb)
performance::check_model(M1_nb)


boot_fun <- function(model, data) {
  # Extract fitted values (mu) and dispersion parameter (theta)
  mu <- fitted(model)  # Mean of the negative binomial distribution
  theta <- model$theta  # Dispersion parameter
  
  # Simulate new response values using the negative binomial distribution
  sim_response <- rnbinom(n = length(mu), size = theta, mu = mu)
  
  # Replace the response column with the simulated data
  data$Use_Frequency <- sim_response
  
  # Fit the model to the simulated data
  sim_model <- update(model, data = data)
  
  # Return the coefficients from the simulated model
  return(coef(sim_model))
}

n_iter <- 1000  # Number of bootstrap iterations
boot_results <- replicate(n_iter, boot_fun(M1_nb, webs_reuse))





boot_results_df <- as.data.frame(t(boot_results))
colnames(boot_results_df) <- names(coef(M1_nb))  # Assign coefficient names
boot_ci <- apply(boot_results_df, 2, function(x) quantile(x, probs = c(0.025, 0.975)))  # 95% CI
boot_ci

## ***********************************************

table_country <- format_glm_table(
  M1,
  caption = "country.mod_continent"
)

table_reuse <- format_glm_table(
  M1_nb,
  caption = "webs_reuse_mod"
)

## ***********************************************
