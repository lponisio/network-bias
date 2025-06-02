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

hist(webs_country$Total_webs_by_country)
hist(webs_country$log_ResInvs_Density)
hist(webs_country$log_CL_Species_density)

## ***********************************************
M1 <- glm.nb(Total_webs_by_country ~ Continent+
               log_ResInvs_Density +
                log_AREA +
                log_CL_Species_density,
                data = webs_country)

summary(M1)
check_model(M1)

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
         !is.na(webs_reuse_count),
         !is.na(Continent),
         years_since_pub<75)

max(webs_reuse$years_since_pub)

#need to only be for actual web_cobe networks
not_in_analysis <- setdiff(unique(webs_complete$Web_Code), unique(webs_reuse$Web_Code))

not_in_analysis <- webs_complete %>%
  distinct(Web_Code, .keep_all = TRUE) %>%
  filter(Web_Code %in%not_in_analysis)


## ***********************************************
write.csv(not_in_analysis[,c("Country", "Web_Code", "webs_reuse_count", "Publi_Year")], file = "network-bias-saved/cleaning/offically_dropped/not_in_analysis_model2.csv")
write.csv(webs_country[,c("adm0_a3","Country","Web_Code","webs_reuse_count")], file = "network-bias-saved/cleaning/offically_dropped/in_analysis_model2.csv")

write_latex_table(not_in_analysis, 
                  file = "network-bias-saved/cleaning/offically_dropped/not_in_analysis_model2.txt", 
                  columns = c("adm0_a3", "Country", "Web_Code"))

write_latex_table(webs_complete[webs_complete$Total_webs_by_country>0,], 
                  file = "network-bias-saved/cleaning/offically_dropped/analysis_model2.txt", 
                  columns = c("adm0_a3", "Country", "Web_Code"))



## ***********************************************

webs_reuse$log_years_since_pub <- datawizard::standardize(webs_reuse$years_since_pub)

library(MASS)
M1_nb <- glm.nb(webs_reuse_count ~ Continent *log_years_since_pub, data = webs_reuse)
summary(M1_nb)
performance::check_model(M1_nb)

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
