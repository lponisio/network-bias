rm(list=ls())
## setwd("~/Dropbox (University of Oregon)/")
setwd("/Volumes/bombus/Dropbox (University of Oregon)")
## setwd("\Dropbox (University of Oregon)")
setwd("C:/Users/emanu/Dropbox (University of Oregon)")

source("network-bias/src/init.R")

## ***********************************************
## Biome models
## ***********************************************
## bayesian version

biome.mod <- brm(bf(Webs ~ log(Area) + Hemisphere),
                 data = biomes_web_data[biomes_web_data$Hemisphere!='Global',],
                 family="negbinomial",
                 chains=3,
                 init=0,
                 iter=10^5)

summary(biome.mod)
AIC(biome.mod)
plot_model(biome.mod)
tab_model(biome.mod)
save(biome.mod, file=file.path(save.path, "biome_mod_negbin.Rdata"))

write.ms.table(biome.mod, "biome_negbin",
               save.path=save.path)


## ***********************************************
## Country models
## ***********************************************

## CHANGE TO NUMBER OF CORES TO RUN MODEL ACROSS
options(cores=5)

## country
source('../network-bias/src/writeResultsTable.R')

country.mod <- brm(bf(Web.count ~
                       scale(log(ResInvestTotal))*Continent +
                       scale(log(AREA))*Continent +
                       scale(log(CL_Species))*Continent),
                      data=gdp_area_species[gdp_area_species$Continent
                                            != "Oceania",],
                   family="negbinomial",
                   chains=3,
                   init=0,
                   iter=10^5)

summary(country.mod)
AIC(country.mod)
plot_model(country.mod)
tab_model(country.mod)
save(country.mod, file=file.path(save.path, "country_mod_negbin.Rdata"))

write.ms.table(country.mod, "country_mod_negbin",
               save.path=save.path)


## country with zero-inflation,  very similar resuls to just negative binomial
country.mod.zero.inf <- brm(bf(Web.count ~
                       scale(log(ResInvestTotal))*Continent +
                       scale(log(AREA))*Continent +
                       scale(log(CL_Species))*Continent),
                      data=gdp_area_species[gdp_area_species$Continent
                                            != "Oceania",],
                   family="zero_inflated_negbinomial",
                   chains=3,
                   init=0,
                   iter=10^5)

summary(country.mod.zero.inf)
AIC(country.mod.zero.inf)
plot_model(country.mod.zero.inf)
tab_model(country.mod.zero.inf)

save(country.mod.zero.inf,
     file=file.path(save.path, "country_mod_zero_inf_negbin.Rdata"))

write.ms.table(country.mod.zero.inf,
               "country_mod_zero_inf_negbin",
               save.path=save.path)




