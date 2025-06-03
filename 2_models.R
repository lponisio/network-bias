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

webs_complete <- webs_complete[!webs_complete$Continent 
                               %in% c("Seven seas (open ocean)"), ]

webs_complete$Continent <- factor(webs_complete$Continent,
                                 levels=c("North America",
                                          "South America",
                                          "Africa",
                                          "Europe",
                                          "Asia",
                                          "Oceania"))

webs_country <- webs_complete %>%
  distinct(adm0_a3, .keep_all = TRUE) %>%
  filter(!is.na(Continent),
           !is.na(AREA) , 
           !is.na(ResInvs_Density) ,
           !is.na(CL_Species_Density))

## ***********************************************
not_in_analysis <- setdiff(unique(webs_complete$adm0_a3), unique(webs_country$adm0_a3))

not_in_analysis <- webs_complete %>%
  distinct(adm0_a3, .keep_all = TRUE) %>%
  filter(adm0_a3 %in%not_in_analysis)

write.csv(not_in_analysis, file = "network-bias-saved/cleaning/offically_dropped/not_in_analysis_model1.csv")
write.csv(webs_country[,c("adm0_a3","Country")], file = "network-bias-saved/cleaning/offically_dropped/in_analysis_model1.csv")

write_latex_table(not_in_analysis, 
                  file = "network-bias-saved/cleaning/offically_dropped/analysis_model1.txt", 
                  columns = c("Country"))



## ***********************************************

# Ensure the dataset contains the required transformed variables
webs_country$log_AREA <- datawizard::standardize(log(webs_country$AREA))
webs_country$log_PropGDP_median <- datawizard::standardize(log(webs_country$PropGDP_median))
webs_country$log_CL_Species <- datawizard::standardize(log(webs_country$CL_Species))
webs_country$log_CL_Species_density <- datawizard::standardize(log(webs_country$CL_Species_Density))
webs_country$log_ResInvs_Density <- datawizard::standardize(log(webs_country$ResInvs_Density))

hist(webs_country[webs_country$Continent=="Africa",]$Total_webs_by_country)
x<-webs_country[webs_country$Continent=="Europe",]

hist(webs_country$log_ResInvs_Density)
hist(webs_country$log_CL_Species_density)


#webs_country<-webs_country[webs_country$Total_webs_by_country<50,]

## ***********************************************
M1 <- glm.nb(Total_webs_by_country ~ Continent+
               log_ResInvs_Density +
                log_AREA +
                log_CL_Species_density,
                data = webs_country)

summary(M1)
check_model(M1)

# Fit zero-inflated negative binomial with centered predictor
M1_nb_zi <- glmmTMB(Total_webs_by_country ~ Continent+
                      log_ResInvs_Density +
                      log_AREA +
                      log_CL_Species_density,
                    ziformula = ~1,
                    family = nbinom2,
                    data = webs_country)

summary(M1_nb_zi)
check_model(M1_nb_zi)


M1_pois <- glm(Total_webs_by_country ~ Continent +
                 log_ResInvs_Density +
                 log_AREA +
                 log_CL_Species_density,
               family = poisson(link = "log"),
               data = webs_country)
summary(M1_pois)
check_model(M1_pois)

#still bad
# M1_pois <- glm(Total_webs_by_country ~ Continent +
#                  log_ResInvs_Density +
#                  log_AREA +
#                  log_CL_Species_density,
#                family = poisson(link = "log"),
#                data = webs_country)
# summary(M1_pois)
# check_model(M1_pois)


#no good
# webs_country$Total_webs_by_country_log <- log(webs_country$Total_webs_by_country + 1)
# library(lme4)
# M1_lmm <- lm(Total_webs_by_country_log ~  Continent+
#                  log_ResInvs_Density +
#                  log_AREA +
#                  log_CL_Species_density,
#                data = webs_country)
# 
# coef_summary <- coef(summary(M1_lmm))
# performance::check_model(M1_lmm)



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
         !is.na(pub_count),
         !is.na(Continent),
         years_since_pub<75)

max(webs_reuse$years_since_pub)

#need to only be for actual web_cobe networks
not_in_analysis <- setdiff(unique(webs_complete$Web_Code), unique(webs_reuse$Web_Code))

not_in_analysis <- webs_complete %>%
  distinct(Web_Code, .keep_all = TRUE) %>%
  filter(Web_Code %in%not_in_analysis)


## ***********************************************
write.csv(not_in_analysis[,c("Country", "Web_Code", "pub_count")], file = "network-bias-saved/cleaning/offically_dropped/not_in_analysis_model2.csv")
write.csv(webs_country[,c("adm0_a3","Country","Web_Code","pub_count")], file = "network-bias-saved/cleaning/offically_dropped/in_analysis_model2.csv")

write_latex_table(not_in_analysis, 
                  file = "network-bias-saved/cleaning/offically_dropped/not_in_analysis_model2.txt", 
                  columns = c("adm0_a3", "Country", "Web_Code"))

write_latex_table(webs_complete[webs_complete$Total_webs_by_country>0,], 
                  file = "network-bias-saved/cleaning/offically_dropped/analysis_model2.txt", 
                  columns = c("adm0_a3", "Country", "Web_Code"))



## ***********************************************

webs_reuse$log_years_since_pub <- datawizard::standardize(webs_reuse$years_since_pub)

library(glmmTMB)
library(performance)

M1_nb <- glmmTMB(pub_count ~ Continent * log_years_since_pub + (1 | Web_Code_base),
                 family = nbinom2,
                 data = webs_reuse)

summary(M1_nb)
performance::check_model(M1_nb)


#the same as nb
# M1_pois <- glmmTMB(pub_count ~ Continent * log_years_since_pub + (1 | Web_Code_base),
#                    family = poisson(),
#                    data = webs_reuse)
# 
# summary(M1_pois)

webs_reuse$log_pub_count <- log(webs_reuse$pub_count + 1)

library(lme4)

M1_lmm <- lmer(log_pub_count ~ Continent * log_years_since_pub + (1 | Web_Code_base),
               data = webs_reuse)

coef_summary <- coef(summary(M1_lmm))
coef_df <- as.data.frame(coef_summary)

coef_df$stars <- cut(coef_df$`Pr(>|t|)`,
                     breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                     labels = c("***", "**", "*", ".", ""),
                     right = TRUE)

coef_df

performance::check_model(M1_lmm)



# #bad
# M1_nb_zi <- glmmTMB(pub_count ~ Continent * log_years_since_pub + (1 | Web_Code_base),
#                     ziformula = ~1,
#                     family = nbinom2,
#                     data = webs_reuse)
# 
# summary(M1_nb_zi)
# performance::check_model(M1_nb_zi)

# #bad
# M1_nb_quad <- glmmTMB(pub_count ~ Continent * log_years_since_pub + I(log_years_since_pub^2) + (1 | Web_Code_base),
#                       family = nbinom2,
#                       data = webs_reuse)
# 
# 
# summary(M1_nb_quad)
# performance::check_model(M1_nb_quad)




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
