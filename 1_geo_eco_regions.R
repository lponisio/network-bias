rm(list=ls())
library(tidyverse)
library(rgdal)
library(maptools)
library(dplyr)

## This script calculates the area for each biome globally and in the
## N and S hemisphere

setwd("~/Dropbox (University of Oregon)/")
## setwd("/Volumes/bombus/Dropbox (University of Oregon)")
## setwd("\Dropbox (University of Oregon)")

setwd("network-bias-saved")

## ***********************************************
## Cleaning country data and GDP
## ***********************************************

webs <- read.csv("Scientiometric_Data_3_jun_2.csv",  sep=";")

gdp <- read.csv("gdp.csv")

## fix issues
webs$Country[webs$Country == "Puerto Rico"] <- "USA"
webs$Country[webs$Country == "Hawaii"] <- "USA"
webs$Country[webs$Country == "New Zealand "] <- "New Zealand"
webs$Country[webs$Country == "Moroco"] <- "Morocco"
webs$Country[webs$Country == "USA"] <- "United States"
webs$Country[webs$Country == "UK"] <- "United Kingdom"
webs$Country[webs$Country == "England"] <- "United Kingdom"
webs$Country[webs$Country == "Venezuela"] <- "Venezuela, RB"
webs$Country[webs$Country == "Egypt"] <- "Egypt, Arab Rep."

## webs without countries
sum(webs$Country == "")

webs$Country[webs$Country == ""] <- NA
webs$ISO3[webs$ISO3 == ""] <- NA

## count up the webs in each country
country.real.dat <- table(webs$ISO3)

## which countries in the data are not in the gdp
names(country.real.dat)[!names(country.real.dat) %in% gdp$Country.Name]

## drop web data without a country
country.real.dat <- country.real.dat[names(country.real.dat) != "#N/A"]

names(country.real.dat)[!names(country.real.dat) %in% gdp$Country.Code]

## remove countries with NA gdp
gdp <- gdp[!is.na(gdp$'X2020'),]

## countries with gdp data but no webs
no.webs <- gdp$Country.Code[!gdp$Country.Code %in%
                            names(country.real.dat)]

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

country.real.dat.gdp <- country.real.dat.gdp[names(country.real.dat.gdp) %in%
                            gdp$Country.Code]

## subset to 2020
gdp.2020 <- gdp[, c("Country.Code", "X2020")]

## alphabetize names
gdp.2020  <- gdp.2020[order(gdp.2020$Country.Code),]

country.real.dat.gdp <- country.real.dat.gdp[order(names(country.real.dat.gdp))]

names(country.real.dat.gdp) == gdp.2020$Country.Code

save(country.real.dat.gdp,
     file="saved/real_country_counts.Rdata")

save(gdp.2020,
     file="saved/GDP_country.Rdata")

write.csv(webs, file="cleaned_web_data.csv",
          row.names=FALSE)



## ***********************************************
## research investment by country
## ***********************************************
res.inv <- read.csv("research_expenditure.csv", sep = ";")

## countries in the research $ data that we have in the studies dataset
 res.inv$Country.Code[res.inv$Country.Code %in%
                      names(country.real.dat.gdp)]

## research $ data but no web data
res.inv$Country.Code[!res.inv$Country.Code %in%
                      names(country.real.dat)]


##
names(studies.by.country)[!names(studies.by.country) %in%
                      res.inv$Country.Code]


## remove countries with NA research investment
res.inv <- res.inv[!is.na(res.inv$'mean2'),]
## match the countires in gdp to web data
res.inv.webs <- res.inv[res.inv$Country.Code %in% names(country.real.dat),]
## match the countries in web data to gdp
country.real.dat <- country.real.dat[names(country.real.dat) %in%
                                       res.inv.webs$Country.Code]
## subset to 2020
res.inv.2018 <- res.inv.webs[, c("Country.Code", "mean2")]
#test
dmultinom(country.real.dat, prob=res.inv.2018$'mean')


## ***********************************************
## Biomes
## ***********************************************

## load biome codes
biome.code <- read.csv("biome_codes.csv")

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
globe.real.dat <- table(webs$BiomeCode)

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
## Area/diversity by country
## ***********************************************
countries <- read.csv("bees_by_country.csv")

bad.countries <- c("", "KP", "KR", "VAT", "FM")
countries <- countries[!countries$ISO3 %in%
                       bad.countries,]

sort(countries$NAME[!countries$NAME %in% webs$Country])

## sort(unique(webs$Country))

#number of networks per country
net.country <- webs %>%
                count(ISO3)

#merging networks and bee richness by country
bee.net <- merge(net.country, countries, by.x='ISO3',
                 by.y='ISO3', all.y = T)

#checking names
countries$ISO3[!(countries$ISO3) %in% bee.net$ISO3]

## net.country <- net.country[!net.country$ISO3 %in%
##                            unique(bee.net$ISO3),]

## set NAs to zero in the network dataset
bee.net$n[is.na(bee.net$n)] <- 0

studies.by.country <- bee.net$n
names(studies.by.country) <- bee.net$ISO3


area.by.country <- bee.net$AREA
area.by.country <- as.numeric(area.by.country)
names(area.by.country) <- bee.net$ISO3

##separating data
bee.div.by.country <- bee.net$CL_Species
names(bee.div.by.country) <-  bee.net$ISO3

bee.div.by.country <- bee.div.by.country[!is.na(bee.div.by.country)]

save(studies.by.country, bee.div.by.country,
     area.by.country,
     file="saved/ISO.Rdata")


save( webs,    file="saved/webs.Rdata")
##

## net.country.decade <- webs %>%
##     group_by(ISO3, Publi_Decade) %>%
##     summarise(Count = length(ISO3))


## net.country.decade <- net.country.decade[net.country.decade$ISO3 !=
##                                          "#N/A",]

## library(vegan)


## This functions takes site-species-abundance data and creates a
## matrix where the sites are columns and the rows are species.

## samp2site.spp <- function(site, spp, abund, FUN=sum) {
##   x <- tapply(abund, list(site = site, spp = spp), FUN)
##   x[is.na(x)] <- 0
##   return(x)
## }

## by.decade <- split(net.country.decade,
##                    net.country.decade$Publi_Decade)

## by.decade <- by.decade[sapply(by.decade, nrow) > 5]

## decade.cat <- unlist(sapply(by.decade, function(x) x$Publi_Decade))
## names(decade.cat) <- NULL

## by.decade.mat <- lapply(by.decade, samp2site.spp, samp2site.spp=

## dist.mat <- vegdist(comm.mat, method= "jaccard",
##                     na.rm=TRUE, diag=TRUE)

## beta.disper.result <- betadisper(dist.mat, GenSp,
##                                  type="centroid")

## ## Permutation test for F and simulate missing values to compare the
## ## differences in the variances of the community composition of
## ## parasites between bee species

## perm.test <- permutest(beta.disper.result,
##           control = permControl(nperm = 100),
##           pairwise = TRUE)

