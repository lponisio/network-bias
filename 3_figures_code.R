#
library(sf)             # spatial data classes
library(rnaturalearth)  # world map data
library(readxl)         # reading excel files
library(dplyr)          # data manipulation
library(tidyr)          # data manipulation
library(purrr)          # data manipulation
library(cartogram)      # cartograms creation
library(tmap)           # maps creation
library(ggplot2)
library(viridis)

#read data
setwd("~/Dropbox (University of Oregon)/")
## setwd("/Volumes/bombus/Dropbox (University of Oregon)")
## setwd("\Dropbox (University of Oregon)")
setwd("C:/Users/emanu/Dropbox (University of Oregon)")

setwd("network-bias-saved")

load(file="../network-bias/data/rawData.Rdata")
load(file="../network-bias-saved/saved/biome_webs.Rdata")
load(file="../network-bias-saved/saved/GDP_web.Rdata")

#graphs

#proportions step 1
#counting the proportions
prop.area.n <- n.area.biome/globe.area.biome
prop.area.s <- s.area.biome/globe.area.biome

#check all proportions together
for(i in 1:14){
     print("*****")
     print(round(dbinom(northern.real.dat[i], global.real.dat[i],
                  prop.area.n[i]), 4))

     print(c(round(northern.real.dat[i]/ global.real.dat[i], 2),
             round(prop.area.n[i], 3)))
     }


## differences in probabilities for biomes area
diffs <- as.data.frame((global.real.dat/sum(global.real.dat) -
         globe.area.biome/sum(globe.area.biome)))

diffs$Biome.name <- biome.code$BiomeName[match(diffs$Var1,
                                              biome.code$BIOME)]

diffs <- diffs[order(diffs$Freq),]

# plot
p <- ggplot(data=diffs, aes(x=Biome.name, y=Freq)) +
     geom_bar(stat="identity") +
     labs(x="", y="Difference in probability based on biome area")
p + coord_flip()

#making the graph prettier

p <- ggplot(data=diffs, aes(x=Biome.name, y=Freq, fill=Freq)) +
  geom_bar(stat="identity")+
  labs(x="", y="Difference in probability based on biome area") +
  theme_minimal()+
  scale_fill_viridis(option = "D")+
  coord_flip() +
  theme(legend.position="none")
p


## differences in probabilities for GDP median
country.real.dat <- as.data.frame(country.real.dat)
webs.gdp <- merge(country.real.dat, gdp.web.dat,
                  by.x = "Var1",
                  by.y = "Country.Code",
                  all = F)

diffs.gdp <- as.data.frame(sort((webs.gdp$Web.count/sum(webs.gdp$Web.count) -
                                   webs.gdp$GDP.MEDIAN/sum(webs.gdp$GDP.MEDIAN))))





## plot
p <- ggplot(data=diffs, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity") +
  labs(x="", y="Difference in probability based on GDP")
p + coord_flip()




#
world_map = ne_countries(returnclass = "sf")

world_map = world_map %>%
  select(gu_a3) %>%
  filter(gu_a3 != "ATA") %>%
  st_transform(world_map, crs = "+proj=robin")

web.loc <- read.csv("../metadata/Scientiometric_Data_3_jun_2.csv", h=T, sep=";")

net.country <- web.loc %>%
  count(ISO3)

world_data = left_join(world_map, net.country, by = c("gu_a3" = "ISO3"))
world_data[is.na(world_data$n),'n'] <- 1

world_carto1 <- cartogram_cont(world_data, "n", maxSizeError = 1.5)
plot(world_carto1["n"])

###################################

world_map = ne_countries(returnclass = "sf")

world_map = world_map %>%
  select(gu_a3) %>%
  filter(gu_a3 != "ATA") %>%
  st_transform(world_map, crs = "+proj=robin")

bees2 <- data.frame(bees$ISO3, bees$CL_Species)

world_data = left_join(world_map, bees2, by = c("gu_a3" = "bees.ISO3")) %>%
  na.omit()


world_carto1 <- cartogram_cont(world_data, "bees.CL_Species", maxSizeError = 1.5)
plot(world_carto1["bees.CL_Species"])

#######################

gdp <- read.csv("../metadata/gdp_total_millions/gdp.csv")

real.gdp <- data.frame(gdp$Country.Code, gdp$X2020)

world_data = left_join(world_map, real.gdp, by = c("gu_a3" = "gdp.Country.Code")) %>%
  na.omit()

world_carto2 <- cartogram_cont(world_data, "gdp.X2020", maxSizeError = 1.5)
plot(world_carto2["gdp.X2020"])

######################
cart <- st_as_sf(cartogram)

ggplot(world_carto2) +
  geom_sf(aes(fill = gdp.X2020), color = "gray30") +
  scale_fill_gradientn(colours = heat.colors(n = 10,
                                             alpha = 0.5,
                                             rev = TRUE))
