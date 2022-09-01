rm(list=ls())
library(tidyverse)
library(rgdal)
library(maptools)
library(dplyr)

#read data

setwd("~/Dropbox (University of Oregon)/")
## setwd("/Volumes/bombus/Dropbox (University of Oregon)")
## setwd("\Dropbox (University of Oregon)")
setwd("C:/Users/emanu/Dropbox (University of Oregon)")

setwd("network-bias-saved")

## ***********************************************
## General cleaning of web data
## ***********************************************

webs <- read.csv("network_papers_2021_2.csv",  sep=";")
res.inv <- read.csv("research_expenditure.csv")
gdp <- read.csv("gdp.csv")
biome.code <- read.csv("biome_codes.csv")
countries <- read.csv("bees_by_country.csv")

#fix issues in country names just to have them correct no longer used for matching data
webs$Country[webs$Country == "puerto rico"] <- "usa"
webs$Country[webs$Country == "hawaii"] <- "usa"
webs$Country[webs$Country == "new zealand "] <- "new zealand"
webs$Country[webs$Country == "moroco"] <- "morocco"
webs$Country[webs$Country == "usa"] <- "united states"
webs$Country[webs$Country == "uk"] <- "united kingdom"
webs$Country[webs$Country == "england"] <- "united kingdom"
webs$Country[webs$Country == "venezuela"] <- "venezuela, rb"
webs$Country[webs$Country == "egypt"] <- "egypt, arab rep."

## ## webs without countries, nothing we can do here
sum(webs$Country == "")
webs$Country[webs$Country == ""] <- NA
webs$iso3[webs$ISO3 == ""] <- NA
webs <- webs[!is.na(webs$Country),]

#
country.real.data <- webs %>%
  group_by(ISO3, Region)%>%
  summarise(webs = n())

## count up the webs in each Country
#country.real.dat <- table(webs$iso3)
#sort(country.real.dat)

## ## all should have a country code now
(country.real.data$ISO3)[!(country.real.data$ISO3) %in%
                         gdp$Country.Code]

countries <- countries[, c("ï..NAME", "ISO3", "AREA", "CL_Species")]

save(webs, res.inv, gdp, biome.code, countries, country.real.dat,
      file="../network-bias/data/rawdata.rdata")

## ***********************************************
## join the web data with the gdp data
## clean gdp data
## **********************************************
load(file="../network-bias/data/rawData.Rdata")

dim(gdp)
## drop the codes for regions, and country groupings
## also drop North Korea because they don't report well
not.real.countries <- c("WLD", "AFE", "AFW", "ARB", "CEB", "CSS", "EAP",
                        "EAR", "EAS", "ECA", "ECS", "EMU", "EUU",
                        "FCS", "HIC", "HPC", "IBD", "IBT", "IDA",
                        "IDB",
                        "IDX", "INX", "LAC", "LCN", "LDC", "LIC",
                        "LMC", "LMY", "LTE", "MEA", "MIC", "MNA",
                        "NAC", "OED", "OSS", "PRE", "PSE", "PSS",
                        "PST",
                        "SAS", "SSA", "SSF", "SST", "TEA", "TEC",
                        "TLA", "TMM", "TSA", "TSS", "UMC", "PRK"
                        )

dim(gdp)- length(not.real.countries)
gdp <- gdp[!gdp$Country.Code %in% not.real.countries,]
dim(gdp)

## we don't want to modify the original data because we will join it
## with other data later on with different missing country data

## which countries in the data are not in the gdp
names(country.real.dat)[!names(country.real.dat) %in% gdp$Country.Name]

## remove countries with NA gdp
gdp <- gdp[!is.na(gdp$'X2020'),]

## countries with gdp data but no webs
no.webs <- gdp$Country.Code[!gdp$Country.Code %in%
                            names(country.real.dat)]

## this is real data, there are no webs from these countries, so
## create 0 count data and add them to the data
no.web.data <- rep(0, length(no.webs))
names(no.web.data)  <- no.webs

country.real.dat <- c(country.real.dat, no.web.data)

## match the countries in web data to gdp

country.real.dat.gdp  <- country.real.dat

## because Greenland is a territory of Denmark, the gdp we would like
## to consider is the Danish GDP
names(country.real.dat.gdp)[names(country.real.dat.gdp) == "GRL"] <-
    "DNK"

country.real.dat.gdp <- tapply(country.real.dat.gdp,
                               names(country.real.dat.gdp), sum)

## double check
gdp$Country.Code[!gdp$Country.Code %in% names(country.real.dat.gdp)]

names(country.real.dat.gdp)[!names(country.real.dat.gdp) %in%
                            gdp$Country.Code]


## we need to drop the webs from VEN and CUB because they don't report
## GDP

country.real.dat.gdp <-
    country.real.dat.gdp[names(country.real.dat.gdp) %in%
                         gdp$Country.Code]


## need the gdp to convert proportion to $$
twty.yrs.gdp <- gdp[, grep("2000",
                         colnames(gdp)):grep("2020",
                                                 colnames(gdp))]

gdp.20.yr.median <- apply(twty.yrs.gdp, 1, median, na.rm=TRUE)

gdp.20.yr.median <- data.frame("GDP.MEDIAN"=gdp.20.yr.median,
                               "Country.Code" =gdp$Country.Code)
rownames(gdp.20.yr.median) <- NULL

gdp <- merge(gdp, gdp.20.yr.median, by="Country.Code")

## subset to median
gdp.median <- gdp[, c("Country.Code", "GDP.MEDIAN")]

## alphabetize names
gdp.median  <- gdp.median[order(gdp.median$Country.Code),]

country.real.dat.gdp <- country.real.dat.gdp[order(
    names(country.real.dat.gdp))]

names(country.real.dat.gdp) == gdp.median$Country.Code

## join gdp data and web count data

gdp.web.dat <- data.frame(
    "Country.Code" =names(country.real.dat.gdp),
    "Web.count" = country.real.dat.gdp)
rownames(gdp.web.dat) <- NULL

gdp.web.dat  <- merge(gdp.web.dat,
                      gdp.median)

head(gdp.web.dat)

save(webs,
     file="saved/webs_raw.Rdata")
save(gdp.web.dat,
     file="saved/GDP_web.Rdata")

write.csv(webs, file="cleaned_web_data.csv",
          row.names=FALSE)

layout(matrix(1:3, ncol=3))
hist(log(gdp.web.dat$GDP.MEDIAN), main="GDP 20 year median", xlab="GDP in US dollars (log)",
     ylab="Number of countries")

#high and low gdp values
#USA
gdp$Country.Code[gdp.web.dat$GDP.MEDIAN == max(gdp.web.dat$GDP.MEDIAN)]
#TUV
gdp$Country.Code[gdp.web.dat$GDP.MEDIAN == min(gdp.web.dat$GDP.MEDIAN)]

## (YAY)

## ***********************************************
## research investment by country
## ***********************************************
dim(res.inv)

res.inv <- res.inv[!res.inv$Country.Code %in%  not.real.countries,]
dim(res.inv)

## take the 20 year median
twty.yrs <- res.inv[, grep("2000",
                         colnames(res.inv)):grep("2020",
                                                 colnames(res.inv))]

## ## need the gdp to convert proportion to $$
## twty.yrs.gdp <- gdp[, grep("2000",
##                          colnames(gdp)):grep("2020",
##                                                  colnames(gdp))]

res.inv$PropGDP_median <- apply(twty.yrs, 1, median, na.rm=TRUE)

## gdp.20.yr.median <- apply(twty.yrs.gdp, 1, median, na.rm=TRUE)

## gdp.20.yr.median <- data.frame("gdp"=gdp.20.yr.median,
##                                "Country.Code" =gdp$Country.Code)
## rownames(gdp.20.yr.median) <- NULL

res.inv <- merge(res.inv, gdp.20.yr.median, by="Country.Code")

## convert proportion to $$ by multiplying gdp and prop
res.inv$ResInvestTotal <- res.inv$PropGDP_median*res.inv$GDP.MEDIAN

## no gdp
res.inv$Country.Code[is.na(res.inv$GDP.MEDIAN)]

## drop NAs
res.inv <- res.inv[!is.na(res.inv$ResInvestTotal),]

## high and low values of research investment
## USA
res.inv$Country.Code[res.inv$ResInvestTotal ==
                     max(res.inv$ResInvestTotal)]

## st. vincent and grenadines
res.inv$Country.Code[res.inv$ResInvestTotal ==
                          min(res.inv$ResInvestTotal)]

## countries in the research $ data that we have in the studies dataset
res.inv$Country.Code[res.inv$Country.Code %in%
                     names(country.real.dat.gdp)]

## research $ data but no web data
res.inv$Country.Code[!res.inv$Country.Code %in%
                     names(country.real.dat.gdp)]

## countries with res investment data but no webs
no.webs <- res.inv$Country.Code[!res.inv$Country.Code %in%
                            names(country.real.dat.gdp)]

no.webs
## since we already fixed this with gdp we are good

## countries with no research investment data but webs
no.res.inv <- names(country.real.dat.gdp)[!names(country.real.dat.gdp) %in%
                            res.inv$Country.Code]
no.res.inv

sort(country.real.dat.gdp[no.res.inv])

#countries with no research investment
country.real.dat.res.inv <-
    country.real.dat.gdp[!names(country.real.dat.gdp) %in% no.res.inv]

## subset to country and research investment data
res.inv.median <- res.inv[, c("Country.Code", "ResInvestTotal")]

## alphabetize names
res.inv.median  <- res.inv.median[order(res.inv.median$Country.Code),]

country.real.dat.res.inv <- country.real.dat.res.inv[order(
    names(country.real.dat.res.inv))]

names(country.real.dat.res.inv) == res.inv.median$Country.Code
## (YAY) all match

res.inv.web.dat <- data.frame(
    "Country.Code" =names(country.real.dat.res.inv),
    "Web.count" = country.real.dat.res.inv)
rownames(res.inv.web.dat) <- NULL

res.inv.web.dat  <- merge(res.inv.web.dat,
                      res.inv.median)
head(res.inv.web.dat)

hist(log(res.inv.web.dat$ResInvestTotal),
     main="Research invenstment 20 year median", xlab="Investment in US dollars (log)",
     ylab="Number of countries")

save(res.inv.web.dat,
     file="saved/res_inv_web.Rdata")


## ***********************************************
## Area and bees' diversity by country
## ***********************************************

## VAT= vatican, FM= micronesia
area.richness <- countries

bad.countries <- c("", "VAT")
area.richness <- area.richness[!area.richness$ISO3 %in%
                       bad.countries,]

sort(unique(area.richness$NAME))

area.richness <- area.richness[, c("NAME", "ISO3",
                                   "AREA", "CL_Species")]

## in gdp
area.richness[area.richness$ISO3 %in% gdp$Country.Code,]

## not in gpd
area.richness[!area.richness$ISO3 %in% gdp$Country.Code,]


## drop really small islands that are territories and would have been
## coded as part of the colonial empire

area.richness$AREA <- as.numeric(area.richness$AREA)

area.richness <- area.richness[area.richness$AREA > 1000,]

## drops countries with no recorded bees

area.richness <-  area.richness[!is.na(area.richness$CL_Species),]

## drop islands off of Antarctica/ Madagascar because they are combined
## but each is less than 1000 km
area.richness <-  area.richness[area.richness$ISO3 != "ATF",]
area.richness <-  area.richness[area.richness$ISO3 != "MDG",]

## drop Western Sahara "disputed territory"
area.richness <-  area.richness[area.richness$ISO3 != "ESH",]


## merging networks and bee richness by country
country.area.web.dat <- data.frame(
    "Country.Code" =names(country.real.dat),
    "Web.count" = country.real.dat)

rownames(country.area.web.dat) <- NULL

country.area.web.dat <- merge(country.area.web.dat,
                              area.richness,
                              by.x = "Country.Code",
                              by.y = "ISO3",
                              all = F)


## NA we count as true zeros. These are countries without GDP for usually
## political reasons but are large areas with bees

country.area.web.dat$Web.count[is.na(country.area.web.dat$Web.count)]  <- 0

## no area data but yes gdp data
country.area.web.dat[is.na(country.area.web.dat$NAME),]

save(country.area.web.dat,
     file="saved/area_richness_web.Rdata")

## ***********************************************
## Biomes
## ***********************************************

## load biome data
biomes <- readOGR(dsn="official", layer="wwf_terr_ecos")

## correct issues with biome names between datasets
biome.code$BiomeName <- toupper(biome.code$BiomeName)
webs$Biome_WWF <- toupper(webs$Biome_WWF)

webs$Biome_WWF <- gsub(",", "", webs$Biome_WWF)
biome.code$BiomeName <- gsub(",", "", biome.code$BiomeName)

biome.code$BiomeName  <- gsub("SCRUB", "SHRUB", biome.code$BiomeName)
biome.code$BiomeName  <- gsub("TEMPERATE CONIFER FORESTS",
                              "TEMPERATE CONIFEROUS FORESTS",
                              biome.code$BiomeName)

biome.code$BiomeName  <- gsub("MANGROVES",
                              "MANGROVE",
                              biome.code$BiomeName)

webs$Biome_WWF[webs$Biome_WWF == "#N/A"] <- NA

unique(webs$Biome_WWF)[!unique(webs$Biome_WWF) %in%
                          biome.code$BiomeName]


webs$BiomeCode <- biome.code$BIOME[match(webs$Biome_WWF,
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
southern.webs <- webs[webs$LAT < 0,]
northern.webs <- webs[webs$LAT > 0,]

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
global.real.dat <- table(webs$BiomeCode)

northern.real.dat <- c(northern.real.dat,
                       "9"=0)

northern.real.dat <-  northern.real.dat[
    as.character(seq(1:14))]

## check we have all the right catagories
length(northern.real.dat) == length(n.area.biome)
## check the order of names
names(northern.real.dat) == names(n.area.biome)

biome.webs <- data.frame("GlobalArea"=globe.area.biome,
                       "BiomeCode" = names(globe.area.biome))

biome.webs$NorthernArea <- n.area.biome[
    match(biome.webs$BiomeCode, names(n.area.biome))]


biome.webs$SouthernArea <- s.area.biome[
    match(biome.webs$BiomeCode, names(s.area.biome))]


biome.webs$NorthernWebs <- northern.real.dat[
    match(biome.webs$BiomeCode, names(northern.real.dat))]


biome.webs$SouthernWebs <- southern.real.dat[
    match(biome.webs$BiomeCode, names(southern.real.dat))]



biome.webs$GlobalWebs <- global.real.dat[
    match(biome.webs$BiomeCode, names(global.real.dat))]


biome.webs$BiomeName <- biome.code$BiomeName[match(
                                       biome.webs$BiomeCode,
                                         biome.code$BIOME)]


save(biome.webs, global.real.dat, northern.real.dat, globe.area.biome,
     file="saved/biome_webs.Rdata")



############
#adding region variable

gdp_area_species$Region <- webs$Region[match(
  gdp_area_species$Country.Code,
  webs$ISO3)]

write.csv(gdp_area_species, file="gdp_area_species_region.csv",
          row.names=FALSE)

gdp_area_species <- read.csv("gdp_area_species_region.csv",  sep=";")

save(gdp_area_species,
     file="../network-bias-saved/saved/gdp_area_species_region.rdata")

