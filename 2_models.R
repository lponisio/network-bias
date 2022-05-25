rm(list=ls())
setwd("~/Dropbox (University of Oregon)/")
## setwd("/Volumes/bombus/Dropbox (University of Oregon)")
## setwd("\Dropbox (University of Oregon)")
setwd("C:/Users/emanu/Dropbox (University of Oregon)")

setwd("network-bias-saved")

data <- read.csv("gdp_area_species", sep = ";")
save(webs,
     file="saved/webs_raw.Rdata")

## dataset
load('saved/area_richness_web.Rdata')
load('saved/biome_webs.Rdata')
load('saved/GDP_web.Rdata')
load('saved/res_inv_web.Rdata')
load('saved/webs_raw.Rdata')

#packages
library(performance)
library(lme4)
library(MuMIn)
library(MASS)
library(car)

## ***********************************************
## AREA by biome
## ***********************************************

## global
# need change variable type
biome.webs$GlobalWebs <- as.numeric(biome.webs$GlobalWebs)

biome.area.mod <- glm(GlobalWebs ~ log(GlobalArea),
                      data=biome.webs, family="poisson")

summary(biome.area.mod)

r.squaredGLMM(biome.area.mod)

check_model(biome.area.mod)



## N hemiphere
biome.area.mod.N <- glm(NorthernWebs ~ log(NorthernArea),
                      data=biome.webs, family="poisson")

summary(biome.area.mod.N)
check_model(biome.area.mod.N)
plot(biome.area.mod.N)


## S hemisphere
biome.area.mod.S <- glm(SouthernWebs ~ log(SouthernArea),
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
                    all = T)

gdp.area.species <- merge(gdp.area.species,
                          res.inv.web.dat,
                          by.x = c("Web.count", "Country.Code"),
                          by.y = c("Web.count", "Country.Code"),
                          all = T)


write.csv(gdp.area.species, file="gdp_area_species.csv",
          row.names=FALSE)

gdp.area.species.2 <- merge(x = gdp.area.species,
                            y = webs[ , c("Country.Code", "Continent")],
                            by = "Country.Code", all.x=T)


all.country.mod <- glm.nb(Web.count ~
                              log(GDP.MEDIAN) +
                              log(AREA) + log(CL_Species),
                              ## log(ResInvestTotal),
                          data=gdp.area.species)

all.country.mod <- glmer(Web.count ~
                              log(GDP.MEDIAN) +
                              #log(ResInvestTotal)+
                              log(AREA) +
                              log(CL_Species) +
                              (1|Country.Code),
                              #(1 + GDP.MEDIAN|Country.Code) +
                              #(1 + CL_Species|Country.Code),
                            ## log(ResInvestTotal),
                            data=gdp.area.species, family = "poisson")

summary(all.country.mod)
check_model(all.country.mod)
plot(all.country.mod)

plot(log(gdp.area.species$GDP.MEDIAN) ~
         log(gdp.area.species$ResInvestTotal))

vif(all.country.mod)

## remove China and the US
outliers <- c("CHN", "USA")


#
