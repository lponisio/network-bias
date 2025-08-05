rm(list=ls())
## ***********************************************
#workingdirectory
source("~/lab_paths.R")
local.path
setwd(local.path)

library(glmmTMB)
library(lmerTest)
library(performance)

source("network-bias/src/initalize_models.R")
## ***********************************************
webs <- webs_complete%>%
  distinct(Web_Code, .keep_all = TRUE)
webs<-webs[!is.na(webs$Web_Code),]


Africa <- webs[webs$Continent=="Africa",]

Africa_summary <- Africa %>%
  count(adm0_a3) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  arrange(desc(percent)) 
unique(Africa$Web_Code_base)

top_countries <- webs %>%
  count(adm0_a3) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  arrange(desc(percent)) %>%
  slice_head(n = 6)
top_countries

## Calculate percent area of top 6 countries
percent_area_top6 <- webs %>%
  distinct(adm0_a3, .keep_all = TRUE) %>%
  summarise(
    total_area_all = sum(AREA, na.rm = TRUE),
    total_area_top6 = sum(AREA[adm0_a3 %in% top_countries$adm0_a3],
                          na.rm = TRUE)
  ) %>%
  mutate(percent_area = total_area_top6 / total_area_all)

print(percent_area_top6$percent_area)

## ***********************************************
## country-level variables
## ***********************************************
# To test whether different country-level variables affected the
# number of networks, we fit a GLM with number of networks collected
# in each country as a response variable and country area (km$^2$),
# research investment, and the number of bee species (the major
# pollinator group in most networks) as explanatory variables. We
# included an interaction with each of these variables and continent
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
no_webs <- webs_country[is.na(webs_country$Web_Code),]

## ***********************************************
not_in_analysis <- setdiff(unique(webs_complete$NAME),
                           unique(webs_country$NAME))

x <- webs_complete[webs_complete$NAME 
                               %in% not_in_analysis, ]

## write.csv(not_in_analysis,
##           file = "network-bias-saved/cleaning/offically_dropped/not_in_analysis_model1.csv")


latex_table <- make_latex_country_table(sort(not_in_analysis,
                                             decreasing =FALSE))
cat(latex_table)


## ***********************************************

# Ensure the dataset contains the required transformed variables
webs_country$log_AREA <-
  datawizard::standardize(log(webs_country$AREA))
webs_country$log_PropGDP_median <-
  datawizard::standardize(log(webs_country$PropGDP_median))
webs_country$log_CL_Species <-
  datawizard::standardize(log(webs_country$CL_Species))
webs_country$log_CL_Species_density <-
  datawizard::standardize(log(webs_country$CL_Species_Density))
webs_country$log_ResInvs_Density <-
  datawizard::standardize(log(webs_country$ResInvs_Density))

hist(webs_country[webs_country$Continent=="Africa",]$Total_webs_by_country)
hist(log(webs_country$Total_webs_by_country+1))

x <- webs_country[webs_country$Continent=="Europe",]

hist(webs_country$log_ResInvs_Density)
hist(webs_country$log_CL_Species_density)

## ***********************************************

glm.1 <- glm(Total_webs_by_country ~ Continent+
               log_ResInvs_Density +
               log_AREA +
               log_CL_Species_density,family = poisson,
             data = webs_country)

## Calculate dispersion stat to confirm the count data is over disperd 
dispersion <- sum(residuals(glm.1, type = "pearson")^2) / df.residual(glm.1)
#if false, need to account for dispersion
2 > dispersion

# This negative binomial accounts for the dispersion
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

# Lastly, to test whether the network re-use was related to the
# country from which the original study collected the network or
# simply the number of years since it has been published, we fit a
# GLMM with the number of times a network was re-used as the response
# variable, and time since the original paper was published and the
# country as explanatory variables. Similar to the second model, we
# included continent as a random effect.
## ***********************************************

webs_reuse <- webs_complete %>%
  filter(!is.na(years_since_pub),
         !is.na(pub_count),
         !is.na(Continent),
         years_since_pub<75)

max(webs_reuse$years_since_pub)

## need to only be for actual web_cobe networks
not_in_analysis <-
  setdiff(unique(webs_complete$Web_Code), unique(webs_reuse$Web_Code))

not_in_analysis <- webs_complete %>%
  distinct(Web_Code, .keep_all = TRUE) %>%
  filter(Web_Code %in%not_in_analysis)


## ***********************************************
## write.csv(not_in_analysis[,c("Country", "Web_Code", "pub_count")],
##           file = "network-bias-saved/cleaning/offically_dropped/not_in_analysis_model2.csv")
## write.csv(webs_country[,c("adm0_a3","Country","Web_Code","pub_count")],
##           file = "network-bias-saved/cleaning/offically_dropped/in_analysis_model2.csv")

## ***********************************************
mean(webs_reuse$pub_count)
sd(webs_reuse$pub_count)
## ***********************************************

webs_reuse$log_years_since_pub <-
  datawizard::standardize(webs_reuse$years_since_pub)

## best fit with the random affect
webs_reuse$log_pub_count <- log(webs_reuse$pub_count + .01)


library(lmerTest)
M1_lmm <- lmerTest::lmer(log_pub_count ~
                           Continent * log_years_since_pub +
                           (1 | Web_Code_base),
               data = webs_reuse)

performance::check_model(M1_lmm)
summary(M1_lmm)

## ***********************************************

table_country <- format_glm_table(
  M1,
  caption = "country.mod_continent"
)

table_reuse <- format_lmer_table(
  M1_lmm,
  caption = "webs_reuse_mod"
)

## ***********************************************
