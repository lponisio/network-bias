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

webs_complete <- webs_complete[!webs_complete$Continent.y 
                               %in% c("Seven seas (open ocean)"), ]

webs_complete$Continent.y <- factor(webs_complete$Continent.y,
                                 levels=c("North America",
                                          "South America",
                                          "Africa",
                                          "Europe",
                                          "Asia", 
                                          "Oceania"))

webs_country <- webs_complete %>%
  distinct(adm0_a3, .keep_all = TRUE) %>%
  filter(!is.na(Continent.y),
           !is.na(AREA) , 
           !is.na(ResInvs_Density) ,
           !is.na(CL_Species_Density))


not_in_analysis <- setdiff(unique(webs_complete$adm0_a3), unique(webs_country$adm0_a3))
not_in_analysis<-webs_complete[webs_complete$adm0_a3 %in%not_in_analysis,]
not_in_analysis <- not_in_analysis %>%
  distinct(adm0_a3, .keep_all = TRUE)

write.csv(not_in_analysis, file = "not_in_analysis.csv")

#countrycode(not_in_analysis, origin = "iso3c", destination = "country.name")


# Ensure the dataset contains the required transformed variables
webs_country$log_AREA <- datawizard::standardize(log(webs_country$AREA))
webs_country$log_PropGDP_median <- datawizard::standardize(log(webs_country$PropGDP_median))
webs_country$log_CL_Species <- datawizard::standardize(log(webs_country$CL_Species))
webs_country$log_CL_Species_density <- datawizard::standardize(log(webs_country$CL_Species_Density))
webs_country$log_ResInvs_Density <- datawizard::standardize(log(webs_country$ResInvs_Density))

hist(webs_country$Total_webs_by_country)
hist(webs_country$log_ResInvs_Density)
hist(webs_country$log_CL_Species_density)


M1 <- glm.nb(Total_webs_by_country ~ Continent.y+
                ResInvs_Density +
                log_AREA +
                log_CL_Species_density,
                data = webs_country)

summary(M1)
check_model(M1)





library(glmmTMB)

M2 <- glmmTMB(Total_webs_by_country ~ Continent +
                log_ResInvs_Density +
                log_AREA +
                log_CL_Species_density,
              #ziformula = ~Continent,
              dispformula = ~log_AREA,
              family = nbinom2,
              data = webs_country)
summary(M2)
check_model(M2)


M_hurdle <- glmmTMB(
  Total_webs_by_country ~ Continent +
    log_ResInvs_Density +
    log_AREA +
    log_CL_Species_density,
  ziformula = Continent +
    log_ResInvs_Density +
    log_AREA +
    log_CL_Species_density,  # No zero-inflation; this is a hurdle model
  #dispformula = ~log_AREA, # Constant dispersion
  family = truncated_nbinom2(),  # Hurdle NB (zeros modeled separately)
  data = webs_country
)

summary(M_hurdle)
check_model(M_hurdle)


library(pscl)

mod.hurdle.nb <- pscl::hurdle(Total_webs_by_country ~ Continent +
                            log_ResInvs_Density +
                            log_AREA +
                            log_CL_Species_density, 
                          data = webs_country, dist = "negbin", 
                          zero.dist = "binomial")

summary(mod.hurdle.nb)
check_model(mod.hurdle.nb)

hist(webs_country[webs_country$Total_webs_by_country>0,]$Total_webs_by_country)

# 
# boot_fun <- function(model, data) {
#   # Extract fitted values (mu) and dispersion parameter (theta)
#   mu <- fitted(model)  # Mean of the negative binomial distribution
#   theta <- model$theta  # Dispersion parameter
#   
#   # Simulate new response values using the negative binomial distribution
#   sim_response <- rnbinom(n = length(mu), size = theta, mu = mu)
#   
#   # Replace the response column with the simulated data
#   data$Total_webs_by_country <- sim_response
#   
#   # Fit the model to the simulated data
#   sim_model <- update(model, data = data)
#   
#   # Return the coefficients from the simulated model
#   return(coef(sim_model))
# }
# 
# 
# n_iter <- 1000  # Number of bootstrap iterations
# boot_results <- replicate(n_iter, boot_fun(M1, webs_country))
# 
# 
# 
# boot_results_df <- as.data.frame(t(boot_results))
# colnames(boot_results_df) <- names(coef(M1))  # Assign coefficient names
# boot_ci <- apply(boot_results_df, 2, function(x) quantile(x, probs = c(0.025, 0.975)))  # 95% CI
# boot_ci







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
         #years_since_pub<90,
         !is.na(webs_reuse_count))
webs_reuse$webs_reuse_count

webs_reuse$log_years_since_pub <- datawizard::standardize(log(webs_reuse$years_since_pub))



library(MASS)
M1_nb <- glm.nb(webs_reuse_count ~ Continent *years_since_pub, data = webs_reuse)
summary(M1_nb)
performance::check_model(M1_nb)

# 
# boot_fun <- function(model, data) {
#   # Extract fitted values (mu) and dispersion parameter (theta)
#   mu <- fitted(model)  # Mean of the negative binomial distribution
#   theta <- model$theta  # Dispersion parameter
#   
#   # Simulate new response values using the negative binomial distribution
#   sim_response <- rnbinom(n = length(mu), size = theta, mu = mu)
#   
#   # Replace the response column with the simulated data
#   data$Use_Frequency <- sim_response
#   
#   # Fit the model to the simulated data
#   sim_model <- update(model, data = data)
#   
#   # Return the coefficients from the simulated model
#   return(coef(sim_model))
# }
# 
# n_iter <- 1000  # Number of bootstrap iterations
# boot_results <- replicate(n_iter, boot_fun(M1_nb, webs_reuse))
# 
# 
# 
# 
# 
# boot_results_df <- as.data.frame(t(boot_results))
# colnames(boot_results_df) <- names(coef(M1_nb))  # Assign coefficient names
# boot_ci <- apply(boot_results_df, 2, function(x) quantile(x, probs = c(0.025, 0.975)))  # 95% CI
# boot_ci

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
