setwd("~/Dropbox (University of Oregon)/")
## setwd("\Dropbox (University of Oregon)")

setwd("Thesis_Emanuelle/Chapter_1-Scientometric/")

## web data for the biome names
web.loc <- read.csv("metadata/Scientiometric_Data_3_jun.csv")

## countrs form web data
load("final_geo_data/real_country_counts.Rdata")
load("final_geo_data/real_biome_counts.Rdata")

## country based on bias hypothesizes
load("final_geo_data/biome_area.Rdata")
load("final_geo_data/GDP_country.Rdata")

## ***********************************************
## AREA by biome
## ***********************************************

## likelihood tests
dmultinom(globe.real.dat, prob=globe.area.biome)
dmultinom(northern.real.dat, prob=n.area.biome)
dmultinom(southern.real.dat, prob=s.area.biome)


## likelihood tests without tundra
dmultinom(globe.real.dat[-11], prob=globe.area.biome[-11])
dmultinom(northern.real.dat[-11], prob=n.area.biome[-11])
dmultinom(southern.real.dat[-9], prob=s.area.biome[-9])

## likelihood tests just in Mediterranean

prop.area.n  <- n.area.biome/globe.area.biome

for(i in 1:14){
    print("*****")
    print(round(dbinom(northern.real.dat[i], globe.real.dat[i],
                 prop.area.n[i]), 4))

    print(c(round(northern.real.dat[i]/ globe.real.dat[i], 2),
            round(prop.area.n[i], 3)))
    }

## differences in probabilities
diffs <- as.data.frame((globe.real.dat/sum(globe.real.dat) -
        globe.area.biome/sum(globe.area.biome)))

diffs$Biome.name <- biome.code$BiomeName[match(diffs$Var1,
                                             biome.code$BIOME)]

diffs <- diffs[order(diffs$Freq),]

## plot
p <- ggplot(data=diffs, aes(x=Biome.name, y=Freq)) +
    geom_bar(stat="identity") +
    labs(x="", y="Difference in probability based on biome area")
p + coord_flip()


## ***********************************************
## GDP by country
## ***********************************************


dmultinom(country.real.dat, prob=gdp.2020$'X2020')

## remove China and the US
outliers <- c("China", "United States")

dmultinom(country.real.dat[!names(country.real.dat) %in% outliers],
          prob=gdp.2020$'X2020'[!gdp.2020$Country.Name %in% outliers])

## differences in probabilities
diffs <- as.data.frame(sort((country.real.dat/sum(country.real.dat) -
        gdp.2020$'X2020'/sum(gdp.2020$'X2020'))))

## plot
p <- ggplot(data=diffs, aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity") +
    labs(x="", y="Difference in probability based on GDP")
p + coord_flip()
