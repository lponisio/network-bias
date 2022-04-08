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

## ***********************************************
## AREA by biome
## ***********************************************
## likelihood tests
dmultinom(biome.webs$GlobalWebs, prob=biome.webs$GlobalArea)
dmultinom(biome.webs$NorthernWebs, prob=biome.webs$NorthernArea)
dmultinom(biome.webs$SouthernWebs[!is.na(biome.webs$SouthernArea)],
          prob=biome.webs$SouthernArea[!is.na(biome.webs$SouthernArea)])

## likelihood tests without tundra
dmultinom(biome.webs$GlobalWebs[biome.webs$BiomeName != "TUNDRA"],
          prob=biome.webs$GlobalArea[biome.webs$BiomeName != "TUNDRA"])
dmultinom(biome.webs$NorthernWebs[biome.webs$BiomeName != "TUNDRA"],
          prob=biome.webs$NorthernArea[biome.webs$BiomeName != "TUNDRA"])
dmultinom(biome.webs$SouthernWebs[!is.na(biome.webs$SouthernArea) &
                                  biome.webs$BiomeName != "TUNDRA"],
          prob=biome.webs$SouthernArea[!is.na(biome.webs$SouthernArea) &
                                       biome.webs$BiomeName != "TUNDRA"])


#counting the proportions
prop.area.n <- n.area.biome/globe.area.biome
prop.area.s <- s.area.biome/globe.area.biome

## ***********************************************
## GDP by country
## ***********************************************

dmultinom(gdp.web.dat$Web.count, prob=gdp.web.dat$GDP.MEDIAN)

## remove China and the US
outliers <- c("CHN", "USA")

## testing without outliers # check why is not working
dmultinom(gdp.web.dat$Web.count[!gdp.web.dat$Web.count %in% outliers],
          prob=gdp.web.dat$GDP.MEDIAN[!gdp.web.dat$Country.Code %in% outliers])


## ***********************************************
## Webs versus research investment by country
## ***********************************************

#check the result with LP
dmultinom(res.inv.web.dat$Web.count, prob = res.inv.web.dat$ResInvestTotal)


## ***********************************************
## Webs versus area by country
## ***********************************************

#check the result with LP
dmultinom(country.area.web.dat$Web.count, prob = country.area.web.dat$AREA)


#since we have the whole data in one object don't we need this?
studies.by.country.area  <-
    country.real.dat[names(country.real.dat)
                       %in% names(area.richness$ISO3)]

area.by.country.studies  <- area.by.country[names(area.by.country)
                             %in% names(studies.by.country.area)]

area.by.country.studies <- area.by.country.studies[
    sort(names(area.by.country.studies))]

studies.by.country.area  <- studies.by.country.area [
    sort(names(studies.by.country.area ))]

dmultinom(studies.by.country.area, prob=area.by.country.studies)

## ***********************************************
## Web versus bee diversity by country
## ***********************************************

dmultinom(country.area.web.dat$Web.count, prob = country.area.web.dat$CL_Species)

## ***********************************************
## Multinomial multiplying probabilities
## ***********************************************
#GDP x Area x Species
gdp.area.species <- merge(gdp.web.dat,
                    country.area.web.dat,
                    by.x = "Country.Code",
                    by.y = "Country.Code",
                    all = F)
gdp.area.species <- gdp.area.species[,-4]
colnames(gdp.area.species)[2] <- "Web.count"

dmultinom(gdp.area.species$Web.count, prob = gdp.area.species$GDP.MEDIAN*
                                              gdp.area.species$AREA*
                                              gdp.area.species$CL_Species)

