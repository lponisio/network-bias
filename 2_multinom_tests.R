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

library(ggplot2)

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


## prop.area.n  <- n.area.biome/globe.area.biome

## for(i in 1:14){
##     print("*****")
##     print(round(dbinom(northern.real.dat[i], globe.real.dat[i],
##                  prop.area.n[i]), 4))

##     print(c(round(northern.real.dat[i]/ globe.real.dat[i], 2),
##             round(prop.area.n[i], 3)))
##     }


## move to plotting code
## ## differences in probabilities
## diffs <- as.data.frame((globe.real.dat/sum(globe.real.dat) -
##         globe.area.biome/sum(globe.area.biome)))

## diffs$Biome.name <- biome.code$BiomeName[match(diffs$Var1,
##                                              biome.code$BIOME)]

## diffs <- diffs[order(diffs$Freq),]

## ## plot
## p <- ggplot(data=diffs, aes(x=Biome.name, y=Freq)) +
##     geom_bar(stat="identity") +
##     labs(x="", y="Difference in probability based on biome area")
## p + coord_flip()


## ***********************************************
## GDP by country
## ***********************************************

dmultinom(gdp.web.dat$Web.count, prob=gdp.web.dat$GDP.MEDIAN)

## remove China and the US
outliers <- c("China", "United States")


## to fix names
dmultinom(country.real.dat[!names(country.real.dat) %in% outliers],
          prob=gdp.2020$'X2020'[!gdp.2020$Country.Name %in% outliers])



## move to plotting code
## differences in probabilities
diffs <- as.data.frame(sort((country.real.dat/sum(country.real.dat) -
        gdp.2020$'X2020'/sum(gdp.2020$'X2020'))))

## plot
p <- ggplot(data=diffs, aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity") +
    labs(x="", y="Difference in probability based on GDP")
p + coord_flip()


## ***********************************************
## Area by country
## ***********************************************

studies.by.country.area  <-
    studies.by.country[names(studies.by.country)
                       %in% names(area.by.country)]

area.by.country.studies  <- area.by.country[names(area.by.country)
                             %in% names(studies.by.country.area)]

area.by.country.studies <- area.by.country.studies[
    sort(names(area.by.country.studies))]

studies.by.country.area  <- studies.by.country.area [
    sort(names(studies.by.country.area ))]

dmultinom(studies.by.country.area, prob=area.by.country.studies)

## ***********************************************
## Bee diversity by country
## ***********************************************

studies.by.country.div  <-
    studies.by.country[names(studies.by.country)
                       %in% names(bee.div.by.country)]

div.by.country.studies  <-
    bee.div.by.country[names(bee.div.by.country)
                       %in% names(studies.by.country.div)]

div.by.country.studies <- div.by.country.studies[
    sort(names(div.by.country.studies))]

studies.by.country.div  <- studies.by.country.div[
    sort(names(studies.by.country.div ))]

dmultinom(studies.by.country.div, prob=div.by.country.studies)
