rm(list=ls())
setwd("~/Dropbox (University of Oregon)/")
## setwd("/Volumes/bombus/Dropbox (University of Oregon)")
## setwd("\Dropbox (University of Oregon)")

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
outliers <- c("China", "United States")

## testing without outliers # check why is not working
dmultinom(gdp.median[!names(gdp.median) %in% outliers],
          prob=gdp.median$GDP.MEDIAN[!gdp.median$Country.Code %in% outliers])


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


