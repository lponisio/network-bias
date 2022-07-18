rm(list=ls())
## setwd("~/Dropbox (University of Oregon)/")
setwd("/Volumes/bombus/Dropbox (University of Oregon)")
## setwd("\Dropbox (University of Oregon)")
## setwd("C:/Users/emanu/Dropbox (University of Oregon)")

source("network-bias/src/init.R")

options(cores=5)
## ***********************************************
## Biome models
## ***********************************************
## bayesian version

biome.mod <- brm(bf(Webs ~ scale(log(Area)) + Hemisphere),
                 data = biomes_web_data[biomes_web_data$Hemisphere!='Global',],
                 family="negbinomial",
                 chains=3,
                 init=0,
                 iter=10^5)

summary(biome.mod)
plot_model(biome.mod)
tab_model(biome.mod)
save(biome.mod,  biomes_web_data,
     file=file.path(save.path, "biome_mod_negbin.Rdata"))

write.ms.table(biome.mod, "biome_negbin",
               save.path=save.path)

load(file="../network-bias/saved/biome_mod_negbin.Rdata")

## ***********************************************
## Country models
## ***********************************************

gdp_area_species <- gdp_area_species[gdp_area_species$Continent
                                     != "Oceania",]

gdp_area_species$Continent  <-  factor(gdp_area_species$Continent,
                                       levels=c("North America",
                                                "Asia", "Africa",
                                                "Europe",
                                                "South America"))

country.mod <- brm(bf(Web.count ~
                        scale(log(ResInvestTotal))*Continent +
                        scale(log(AREA))*Continent +
                        scale(log(CL_Species))),
                   data=gdp_area_species,
                   family="negbinomial",
                   chains=3,
                   init=0,
                   iter=10^5)

summary(country.mod)
plot_model(country.mod)
tab_model(country.mod)
save(country.mod, gdp_area_species,
     file=file.path(save.path, "country_mod_negbin.Rdata"))

write.ms.table(country.mod,  "country_mod_negbin",
               save.path=save.path)

load(file="../network-bias/saved/country_mod_negbin.Rdata")


## country with zero-inflation,  very similar resuls to just negative binomial
country.mod.zero.inf <- brm(bf(Web.count ~
                                 scale(log(ResInvestTotal))*Continent +
                                 scale(log(AREA))*Continent +
                                 scale(log(CL_Species))*Continent),
                            data=gdp_area_species,
                            family="zero_inflated_negbinomial",
                            chains=3,
                            init=0,
                            iter=10^5)

summary(country.mod.zero.inf)
plot_model(country.mod.zero.inf)
tab_model(country.mod.zero.inf)

save(country.mod.zero.inf, gdp_area_species,
     file=file.path(save.path, "country_mod_zero_inf_negbin.Rdata"))

write.ms.table(country.mod.zero.inf,
               "country_mod_zero_inf_negbin",
               save.path=save.path)




## ***********************************************
## Re-use models
## ***********************************************

reused.web <- reused.web[reused.web$Continent
                         != "Oceania",]

reused.web$Continent  <-  factor(reused.web$Continent,
                                 levels=c("North America",
                                          "Asia", "Africa",
                                          "Europe",
                                          "South America"))

reuse.mod <- brm(bf(ReusedCount ~
                      scale(Age)*Continent +
                      From_Database),
                 data=reused.web,
                 family="negbinomial",
                 chains=3,
                 init=0,
                 iter=10^5)

summary(reuse.mod)
plot_model(reuse.mod)
tab_model(reuse.mod)
save(reuse.mod, reused.web,
     file=file.path(save.path, "reuse_mod_negbin.Rdata"))

write.ms.table(reuse.mod,  "reuse_mod_negbin",
               save.path=save.path)

load(file="../network-bias/saved/reuse_mod_negbin.Rdata")

## zero inflated, really similar to the results above

reuse.mod.zero.inf <- brm(bf(ReusedCount ~
                               scale(Age)*Continent +
                               From_Database),
                          data=reused.web,
                          family="zero_inflated_negbinomial",
                          chains=3,
                          init=0,
                          iter=10^5)

summary(reuse.mod.zero.inf)
plot_model(reuse.mod.zero.inf)
tab_model(reuse.mod.zero.inf)
save(reuse.mod.zero.inf, reused.web,
     file=file.path(save.path, "reuse_mod_zero_inf_negbin.Rdata"))

write.ms.table(reuse.mod.zero.inf,  "reuse_mod_zero_inf_negbin",
               save.path=save.path)

conditional_effects(biome.mod)
conditional_effects(country.mod)

library(tidyverse)
library(styler)
library(ggExtra)
library(knitr)
library(brms)
library(cowplot)
library(gridExtra)
library(skimr)
library(DiagrammeR)
library(rayshader)
library(av)
library(rgl)
library(tibble)

post_samplesM1_tbl <-
  posterior_samples(biome.mod) %>%
  select(-lp__) %>%
  round(digits = 3)

post_samplesM1_tbl %>%
  head(10) %>%
  kable(align = rep("c", 3))

mod_1_summary_tbl <-
  posterior_summary(biome.mod) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  as_tibble() %>%
  mutate_if(is.numeric, funs(as.character(signif(., 2)))) %>%
  mutate_at(.vars = c(2:5), funs(as.numeric(.)))
mod_1_summary_tbl %>%
  kable(align = rep("c", 5))


mean_regressionM1_fig <-
  biomes_web_data[biomes_web_data$Hemisphere!='Global',] %>%
  ggplot(aes(y = Webs, x = Area)) +
  geom_point(
    colour = "#481567FF",
    size = 2,
    alpha = 0.6
  ) +
  geom_abline(aes(intercept = b_Intercept, slope = b_scalelogArea),
              data = post_samplesM1_tbl,
              alpha = 0.1, color = "gray50"
  ) +
  geom_abline(
    slope = mean(post_samplesM1_tbl$b_scalelogArea),
    intercept = mean(post_samplesM1_tbl$b_Intercept),
    color = "blue", size = 1
  ) +
  labs(
    x = "Area per biome",
    y = "Networks"
  )


webs %>% mutate(Hemisphere =
                     case_when(LAT > 0 ~ "Northern",
                               LAT < 0 ~ "Southern"),
                .after=LONG)



