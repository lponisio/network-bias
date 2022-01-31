rm(list=ls())
library(tidyverse)
library(rgdal)
library(maptools)
library(dplyr)

## This script calculates the area for each biome globally and in the
## N and S hemisphere

## setwd("~/Dropbox (University of Oregon)/")
setwd("/Volumes/bombus/Dropbox (University of Oregon)")
## setwd("\Dropbox (University of Oregon)")

setwd("network-bias-saved")

## ***********************************************
## Cleaning country data and GDP
## ***********************************************

web.loc <- read.csv("Scientiometric_Data_3_jun_2.csv",  sep=";")

gdp <- read.csv("gdp.csv")

## fix issues
web.loc$Country[web.loc$Country == "Puerto Rico"] <- "USA"
web.loc$Country[web.loc$Country == "Hawaii"] <- "USA"

web.loc$Country[web.loc$Country == "New Zealand "] <- "New Zealand"

## webs without countries
sum(web.loc$Country == "")

web.loc$Country[web.loc$Country == ""] <- NA

## count up the webs in each country
country.real.dat <- table(web.loc$Country)

## which countries in the data are not in the gdp
names(country.real.dat)[!names(country.real.dat) %in% gdp$Country.Name]

web.loc$Country[web.loc$Country == "Moroco"] <- "Morocco"
web.loc$Country[web.loc$Country == "USA"] <- "United States"
web.loc$Country[web.loc$Country == "UK"] <- "United Kingdom"
web.loc$Country[web.loc$Country == "England"] <- "United Kingdom"
web.loc$Country[web.loc$Country == "Venezuela"] <- "Venezuela, RB"
web.loc$Country[web.loc$Country == "Egypt"] <- "Egypt, Arab Rep."

## because Greenland is a territory of Denmark, the gdp we would like
## to consider is the Danish GDP
web.loc$Country[web.loc$Country == "Greenland"] <- "Denmark"

## count up the webs in each country
country.real.dat <- table(web.loc$Country)

names(country.real.dat)[!names(country.real.dat) %in% gdp$Country.Name]

## remove countries with NA gdp
gdp <- gdp[!is.na(gdp$'X2020'),]

## match the countires in gdp to web data
gdp.webs <- gdp[gdp$Country.Name %in% names(country.real.dat),]

## match the countries in web data to gdp

country.real.dat <- country.real.dat[names(country.real.dat) %in%
                                 gdp.webs$Country.Name]

## subset to 2020
gdp.2020 <- gdp.webs[, c("Country.Name", "X2020")]

## alphabetize names
gdp.2020  <- gdp.2020[order(gdp.2020$Country.Name),]
country.real.dat <- country.real.dat[order(names(country.real.dat))]

names(country.real.dat) == gdp.2020$Country.Name

save(country.real.dat,
     file="saved/real_country_counts.Rdata")

save(gdp.2020,
     file="saved/GDP_country.Rdata")

write.csv(web.loc, file="cleaned_web_data.csv",
          row.names=FALSE)


## ***********************************************
## Biomes
## ***********************************************

## load biome codes
biome.code <- read.csv("biome_codes.csv")

## load biome data
biomes <- readOGR(dsn="official", layer="wwf_terr_ecos")

## correct issues with biome names between datasets
biome.code$BiomeName <- toupper(biome.code$BiomeName)
web.loc$Biome_WWF <- toupper(web.loc$Biome_WWF)

web.loc$Biome_WWF <- gsub(",", "", web.loc$Biome_WWF)
biome.code$BiomeName <- gsub(",", "", biome.code$BiomeName)

biome.code$BiomeName  <- gsub("SCRUB", "SHRUB", biome.code$BiomeName)
biome.code$BiomeName  <- gsub("TEMPERATE CONIFER FORESTS",
                              "TEMPERATE CONIFEROUS FORESTS",
                              biome.code$BiomeName)

biome.code$BiomeName  <- gsub("MANGROVES",
                              "MANGROVE",
                              biome.code$BiomeName)

web.loc$Biome_WWF[web.loc$Biome_WWF == "#N/A"] <- NA

unique(web.loc$Biome_WWF)[!unique(web.loc$Biome_WWF) %in%
                          biome.code$BiomeName]


web.loc$BiomeCode <- biome.code$BIOME[match(web.loc$Biome_WWF,
                                             biome.code$BiomeName)]

## sum the area for each biome in the S, N and the entire globe

## drop codes that are for missing data
biomes <- biomes[!biomes@data$BIOME %in% c(98,99),]

## sum the area in each biome

southern <- biomes[coordinates(biomes)[,2] < 0,]
northern <- biomes[coordinates(biomes)[,2] > 0,]


n.area.biome <- tapply(northern@data$area_km2, northern@data$BIOME,
                       sum)
s.area.biome <- tapply(southern@data$area_km2, southern@data$BIOME,
                       sum)
globe.area.biome <-  tapply(biomes@data$area_km2, biomes@data$BIOME,
                       sum)

## southern hemisphere
southern.webs <- web.loc[web.loc$LAT < 0,]
northern.webs <- web.loc[web.loc$LAT > 0,]

southern.real.dat <- table(southern.webs$BiomeCode)
southern.real.dat <- c(southern.real.dat,
                       "14"=0, "11"=0, "3"=0)

southern.real.dat <-  southern.real.dat[
    c("1", "2", "3", "4", "7", "8", "9",
      "10", "11", "12", "13", "14")]

## check we have all the right catagories
length(southern.real.dat) == length(s.area.biome)
## check the order of names
names(southern.real.dat) == names(s.area.biome)

## northern hemisphere
northern.real.dat <- table(northern.webs$BiomeCode)
globe.real.dat <- table(web.loc$BiomeCode)

northern.real.dat <- c(northern.real.dat,
                       "9"=0)

northern.real.dat <-  northern.real.dat[
    as.character(seq(1:14))]

## check we have all the right catagories
length(northern.real.dat) == length(n.area.biome)
## check the order of names
names(northern.real.dat) == names(n.area.biome)

save(n.area.biome, s.area.biome, globe.area.biome,
     file="saved/biome_area.Rdata")

save(northern.real.dat, southern.real.dat, globe.real.dat,
     file="saved/real_biome_counts.Rdata")

## ***********************************************
## Area by country
## ***********************************************
countries <- read.csv("bees_by_country.csv")

sort(countries$NAME[!countries$NAME %in% web.loc$Country])

sort(unique(web.loc$Country))

#number of networks per country
net.country <- web.loc %>%
                count(ISO3)

#merging networks and bee richness by country
bee.net <- merge(net.country, countries, by.x='ISO3',
                 by.y='ISO3', all.y = T)

#checking names
names(countries)[!names(countries) %in% bee.net$ISO3]

## net.country <- net.country[!net.country$ISO3 %in%
##                            unique(bee.net$ISO3),]

## set NAs to zero in the network dataset
bee.net$n[is.na(bee.net$n)] <- 0

studies.by.country <- bee.net$n
save(studies.by.country,
     file="saved/study_counts_ISO.Rdata")

## drop countries without bee species richness data
bee.net <- bee.net[!is.na(bee.net$CL_Species),]

#separating data
bee.div.by.country <- bee.net$CL_Species

save(bee.div.by.country,
     file="saved/bee_div_ISO.Rdata")


