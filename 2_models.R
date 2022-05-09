rm(list=ls())
setwd("~/Dropbox (University of Oregon)/")
## setwd("/Volumes/bombus/Dropbox (University of Oregon)")
## setwd("\Dropbox (University of Oregon)")
## setwd("C:/Users/emanu/Dropbox (University of Oregon)")

setwd("network-bias-saved")

## biome.webs
load('saved/area_richness_web.Rdata')
load('saved/biome_webs.Rdata')
load('saved/GDP_web.Rdata')
load('saved/res_inv_web.Rdata')

library(performance)
library(lme4)
library(MuMIn)
library(MASS)

## ***********************************************
## AREA by biome
## ***********************************************

## global
biome.area.mod <- glm(GlobalWebs ~ log(GlobalArea),
                      data=biome.webs, family="poisson")

summary(biome.area.mod)

r.squaredGLMM(biome.area.mod)

performance::check_model(biome.area.mod)


## N hemiphere
biome.area.mod.N <- glm(NorthernWebs ~ log(GlobalArea),
                      data=biome.webs, family="poisson")

summary(biome.area.mod.N)


## S hemisphere
biome.area.mod.S <- glm(SouthernWebs ~ log(GlobalArea),
                      data=biome.webs, family="poisson")

summary(biome.area.mod.S)

## ***********************************************
## GDP by country
## ***********************************************

#GDP x Area x Species
gdp.area.species <- merge(gdp.web.dat,
                    country.area.web.dat,
                    by.x = c("Web.count", "Country.Code"),
                    by.y = c("Web.count", "Country.Code"),
                    all = F)

gdp.area.species <- merge(gdp.area.species,
                          res.inv.web.dat,
                          by=c("Web.count", "Country.Code"))


all.country.mod <- glm.nb(Web.count ~
                              log(GDP.MEDIAN) +
                              log(AREA) + log(CL_Species),
                              ## log(ResInvestTotal),
                          data=gdp.area.species)

summary(all.country.mod)


plot(log(gdp.area.species$GDP.MEDIAN) ~
         log(gdp.area.species$ResInvestTotal))

vif(all.country.mod)

## remove China and the US
outliers <- c("CHN", "USA")


