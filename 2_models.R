rm(list=ls())
## ***********************************************
setwd(network-bias)

source("src/initalize_models.R")

## ***********************************************

webs <- webs_complete%>%
  distinct(Web_Code, .keep_all = TRUE)
webs<-webs[!is.na(webs$Web_Code),]


webs$FirstAuthor <- sapply(strsplit(webs$Web_Code, "_"),
                                    function(x) x[1])

##  Summary stats for ms

## Countries with data in Africa
Africa <- webs[webs$Continent=="Africa",]

Africa_summary <- Africa %>%
  count(adm0_a3) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  arrange(desc(percent)) 
unique(Africa$Web_Code_base)


##  Get the top 6 countries by number of networks
top_countries <- webs %>%
  count(adm0_a3, name = "n_networks") %>%
  mutate(percent = 100 * n_networks / sum(n_networks)) %>%
  arrange(desc(n_networks)) %>%
  slice_head(n = 6) 
top_countries

# Count top 3 first authors within each of those countries
top_authors_by_country <- webs %>%
  filter(adm0_a3 %in% top_countries$adm0_a3) %>%
  count(adm0_a3, FirstAuthor, Continent,  name = "n_networks") %>%
  group_by(adm0_a3, Continent) %>%
  arrange(desc(n_networks)) %>%
  slice_head(n = 3) %>%
  ungroup()
top_authors_by_country


# Count top 3 first authors in Global South countries
top_authors_global_S <- webs %>%
  filter(Hemisphere == "Southern") %>%
  count(adm0_a3, Continent, FirstAuthor, name = "n_networks") %>%
  group_by(adm0_a3, Continent) %>%
  arrange(desc(n_networks)) %>%
  slice_head(n = 5) %>%
  ungroup()
top_authors_global_S


##  Calculate percent area of top 6 countries
percent_area_top6 <- webs %>%
  distinct(adm0_a3, .keep_all = TRUE) %>%
  summarise(
    total_area_all = sum(AREA, na.rm = TRUE),
    total_area_top6 = sum(AREA[adm0_a3 %in% top_countries$adm0_a3],
                          na.rm = TRUE)
  ) %>%
  mutate(percent_area = total_area_top6 / total_area_all)

print(percent_area_top6$percent_area)

## ***********************************************************
## country-level variables
## ***********************************************************
# To test whether different country-level variables affected the
# number of networks, we fit a GLM with number of networks collected
# in each country as a response variable and country area (km$^2$),
# research investment, and the number of bee species (the major
# pollinator group in most networks) as explanatory variables. We
# included an interaction with each of these variables and continent
# to allow their slopes to vary by continent.
## ***********************************************************

## Drop countries considered part of global oceans because it is not a
## distinguishable continent
webs_complete <- webs_complete[!webs_complete$Continent 
                               %in% c("Seven seas (open ocean)"), ]

webs_complete$Continent <- factor(webs_complete$Continent,
                                  levels=c("North America",
                                           "South America",
                                           "Africa",
                                           "Europe",
                                           "Asia",
                                           "Oceania"))

## Drop countries without complete data
webs_country <- webs_complete %>%
  distinct(adm0_a3, .keep_all = TRUE) %>%
  filter(!is.na(Continent),
         !is.na(AREA) , 
         !is.na(ResInvs_Density) ,
         !is.na(CL_Species_Density))

## Countries without any networks collected
no_webs <- webs_country[is.na(webs_country$Web_Code),]
sort(no_webs$Country)

## ***********************************************************
## Webs excluded from analysis due to not meeting 1+ inclusion
## criteria

not_in_analysis <- setdiff(unique(webs_complete$NAME),
                           unique(webs_country$NAME))

latex_table <- make_latex_country_table(sort(not_in_analysis,
                                             decreasing =FALSE))
cat(latex_table)

## ***********************************************************
# Standardize variables and visualize their distributions

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
hist(log(webs_country$Total_webs_by_country + 1))

hist(webs_country$log_ResInvs_Density)
hist(webs_country$log_CL_Species_density)

# This negative binomial model accounts for dispersion
network_use <- glm.nb(Total_webs_by_country ~ Continent+
               log_ResInvs_Density +
                log_AREA +
                log_CL_Species_density,
                data = webs_country)

summary(network_use)
check_model(network_use)

## ***********************************************
## network re-use
## ***********************************************

# Lastly, to test whether the network re-use was related to the
# country from which the original study collected the network or
# simply the number of years since it has been published, we fit a
# GLMM with the number of times a network was re-used as the response
# variable, and time since the original paper was published and the
# country as explanatory variables. We included study as a random
# effect.
## ***********************************************

webs_reuse <- webs_complete %>%
  filter(!is.na(years_since_pub),
         !is.na(pub_count),
         !is.na(Continent),
         years_since_pub < 75)

max(webs_reuse$years_since_pub)

## need to only be for actual web_cobe networks
not_in_analysis <-
  setdiff(unique(webs_complete$Web_Code), unique(webs_reuse$Web_Code))

not_in_analysis <- webs_complete %>%
  distinct(Web_Code, .keep_all = TRUE) %>%
  filter(Web_Code %in%not_in_analysis)

## summary stats for ms
mean(webs_reuse$pub_count)
sd(webs_reuse$pub_count)

mean(webs_reuse$years_since_pub)
## ***********************************************

webs_reuse$log_years_since_pub <-
  datawizard::standardize(webs_reuse$years_since_pub)

webs_reuse$log_pub_count <- log(webs_reuse$pub_count + .01)

reuse_mod <- lmerTest::lmer(log_pub_count ~
                           Continent * log_years_since_pub +
                           (1 | Web_Code_base),
               data = webs_reuse)

performance::check_model(reuse_mod)
summary(reuse_mod)

## ***********************************************
## export results
table_country <- format_glm_table(
  network_use,
  caption = "country.mod_continent"
)

table_reuse <- format_lmer_table(
  reuse_mod,
  caption = "webs_reuse_mod"
)

## ***********************************************
