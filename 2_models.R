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
webs_country <- webs_complete %>%
  distinct(ISO3, .keep_all = TRUE) %>%
  filter(!is.na(Continent) & 
           !is.na(AREA) & 
           !is.na(PropGDP_median) & 
           !is.na(CL_Species))

webs_country <- webs_country[webs_country$Continent != "Oceania",]

webs_country$Continent <- factor(webs_country$Continent,
                                     levels=c("Northern America",
                                              "Southern America",
                                              "Africa",
                                              "Europe",
                                              "Asia"))

# Ensure the dataset contains the required transformed variables
webs_country$log_AREA <- scale(log(webs_country$AREA))
webs_country$log_PropGDP_median <- scale(log(webs_country$PropGDP_median))
webs_country$log_CL_Species <- scale(log(webs_country$CL_Species))


M1 <- MASS::glm.nb(Total_webs_by_country ~ Continent +
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






#######






#this doesn't work because negative binomial 
#library(pbkrtest)
#pb <- PBmodcomp(country.mod.large, country.mod.small)
#pb

# 
# # # Fit zero-inflated negative binomial model
#  country.mod.zi <- glmmTMB(
#    Total_webs_by_country ~ Continent +
#      scale(log(AREA)) +
#      scale(log(PropGDP_median)) +
#      scale(log(CL_Species)),      
#     ziformula = ~ 1,          
#    family = nbinom2,                 
#    data = webs_country
#  )
# # 
# # # Check model diagnostics
#   performance::check_model(country.mod.zi)
# # # Simulate residuals and plot diagnostics
# # sim_res_zi <- simulateResiduals(country.mod.zi)
# # plot(sim_res_zi)
# # 
# # # Summarize the model
#  summary(country.mod.zi)

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
  distinct(Web_Code, .keep_all = TRUE) %>%
  filter(!is.na(years_since_pub),
         years_since_pub<90)


library(glmmTMB)
# Model with variable dispersion
webs_reuse_mod <- glmmTMB(Use_Frequency ~ ISO3 + years_since_pub,
                          data = webs_reuse, ziformula = ~ 1, 
                          family = nbinom2
                         )

summary(webs_reuse_mod)

performance::check_model(webs_reuse_mod)

years_since_pub

## ***********************************************
#gonna put this in a function/source file later but I am trying to tidy up my 
#latex table code to automatically populate results


#table_biome <- format_glm_table(
#  biome.net.m2,
#  caption = "biome.net.m2"
#)

table_country <- format_glm_table(
  M1,
  caption = "country.mod_continent"
)

table_reuse <- format_glm_table(
  webs_reuse_mod,
  caption = "webs_reuse_mod"
)

# Extract ISO3 codes (matching 'ISO3XXX' pattern)
iso3_codes <- unique(regmatches(table_reuse, gregexpr("ISO3[A-Z]{3}", table_reuse))[[1]])

# Remove 'ISO3' prefix to get the actual ISO3 codes
iso3_clean <- gsub("ISO3", "", iso3_codes)

# Convert to country names
country_names <- countrycode(iso3_clean, origin = "iso3c", destination = "country.name")

# Create a mapping of ISO3 to country names
iso3_to_country <- setNames(country_names, iso3_codes)

# Replace each ISO3 code with its corresponding country name
for (iso3 in names(iso3_to_country)) {
  table_reuse <- gsub(iso3, iso3_to_country[[iso3]], table_reuse)
}


## ***********************************************
#We are no longer analyzing biome because all the other metrics are at the country
#level
## ***********************************************


## ***********************************************
## Biome models
## ***********************************************
## from manuscript currently
# To test whether network collection is related to the biome area in the Northern 
# and Southern hemispheres, we included the number of networks by country as a 
# response variable and the biome area as an explanatory variable in a generalized 
# linear model (GLM).  We included an interaction between biome area and Northern 
# vs. Southern hemispehere to allow the accumulation of network with are to vary 
# by hemisphere. 
## ***********************************************
#we now have 
# webs_bycounty <- webs_complete %>%
#   filter(BiomeCode != "NA")%>%
#   distinct(ISO3, .keep_all = TRUE) 
# 
# biome.net.m2 <- glm(Total_webs_by_country ~ log(AREA_biome_total) * Hemisphere,
#                     data = webs_bycounty,
#                     family = "poisson")
# summary(biome.net.m2)
# 
# performance::check_model(biome.net.m2)
