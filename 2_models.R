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
  filter(!is.na(CL_Species))

webs_country <- webs_country[webs_country$Continent != "Oceania",]

webs_country$Continent <- factor(webs_country$Continent,
                                     levels=c("Northern America",
                                              "Southern America",
                                              "Africa",
                                              "Europe",
                                              "Asia"))


# Identify missing values
colSums(is.na(webs_country))

# Check for problematic log-transformed values
summary(log(webs_country$AREA))
summary(log(webs_country$PropGDP_median))
summary(log(webs_country$CL_Species))


# Drop rows with NAs in model-relevant columns
webs_country_clean <- webs_country %>%
  dplyr::filter(!is.na(Continent) & !is.na(AREA) & !is.na(PropGDP_median) & !is.na(CL_Species))

# Check again for missing values
colSums(is.na(webs_country_clean))


#negative binomial
M1 <- glm.nb(Total_webs_by_country ~ Continent +
                                  scale(log(AREA)) +
                                  scale(log(PropGDP_median)) +
                                  scale(log(CL_Species)),
                                  data = webs_country)
nboot <- 1000
bres <- matrix(NA,nrow=nboot,
               ncol=length(coef(M1)),
               dimnames=list(rep=seq(nboot),
                             coef=names(coef(M1))))

set.seed(101)
bootsize <- 200
for (i in seq(nboot)) {
  bdat <- webs_country[sample(nrow(webs_country),size=bootsize,replace=TRUE),]
  bfit <- update(M1, data=bdat)  ## refit with new data
  bres[i,] <- coef(bfit)
}

data.frame(mean_est=colMeans(bres),
           t(apply(bres,2,quantile,c(0.025,0.975))))

country.mod.small <- glm.nb(Total_webs_by_country ~ Continent +
                              scale(log(AREA)) +
                              scale(log(PropGDP_median)) +
                              scale(log(CL_Species)),
                            data = webs_country)

#sequential test, may inflate the significance of varibales added "early"
anova(country.mod.large)
# Compare the models using a likelihood ratio test
anova(country.mod.large, country.mod.small, test = "Chisq")

summary(country.mod.large)

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
  mutate(years_since_pub = as.numeric(format(Sys.Date(), "%Y")) - Publi_Year)

#ARG is the intercept :)
webs_reuse_mod <- glm(Use_Frequency ~ ISO3 +years_since_pub,
                             data=webs_reuse,
                             ## ziformula = ~1,
                             family = "poisson")

summary(webs_reuse_mod)

performance::check_model(webs_reuse_mod)



## ***********************************************
#gonna put this in a function/source file later but I am trying to tidy up my 
#latex table code to automatically populate results


table_biome <- format_glm_table(
  biome.net.m2,
  caption = "biome.net.m2"
)

table_country <- format_glm_table(
  country.mod_continent,
  caption = "country.mod_continent"
)

table_reuse <- format_glm_table(
  webs_reuse_mod,
  caption = "webs_reuse_mod"
)




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
