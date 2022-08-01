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
## setwd("C:/Users/emanu/Dropbox (University of Oregon)")

setwd("network-bias-saved")

load(file="../network-bias/data/rawData.Rdata")
load(file="../network-bias-saved/saved/biome_webs.Rdata")
load(file="../network-bias-saved/saved/GDP_web.Rdata")
load(file="../network-bias-saved/saved/res_inv_web.Rdata")
load(file="../network-bias-saved/saved/area_richness_web.Rdata")

#graphs

#general box plot for networks' latitude
#violin plot
p <- ggplot(webs, aes(x = "", y=LAT)) +
  geom_violin(trim=FALSE, fill="gray") +
  labs(x=NULL, y = "Latitude") +
  geom_boxplot(width=0.1)+
  scale_y_continuous(breaks = seq(-50, 100, by = 25)) +
  #geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_color_grey() +
  theme_classic()
p

tiff('nets_lat.tif', w=3000, h=6000, units="px", res=600, compression = "lzw")
p
dev.off()

#by decade box plot for networks' latitude
#violin plot
summary(webs)
webs$Publi_Decade <- as.factor(webs$Publi_Decade)

p <- ggplot(webs, aes(x = "", y = LAT)) +
  geom_boxplot(width=0.5) +
  labs(x="General", y = "Latitude")+
  scale_y_continuous(breaks = seq(-50, 100, by = 25)) +
  geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.3) +
  scale_color_grey() +
  theme_classic()

tiff('nets_lat_general.tif', w=1500, h=3000, units="px", res=600, compression = "lzw")
p
dev.off()

g <- ggplot(webs, aes(x = Publi_Decade, y = LAT)) +
  geom_boxplot(width=0.5) +
  labs(x="Years", y = "Latitude") +
  scale_y_continuous(breaks = seq(-50, 100, by = 25)) +
  geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.3) +
  scale_color_grey() +
  theme_classic()

tiff('nets_lat_decade.tif', w=6000, h=3000, units="px", res=600, compression = "lzw")
g
dev.off()

p <- ggplot(webs, aes(x = Publi_Decade, y=LAT)) +
  geom_violin(trim=FALSE, fill="gray") +
  labs(x=NULL, y = "Latitude") +
  geom_boxplot(width=0.1)+
  scale_y_continuous(breaks = seq(-50, 100, by = 25)) +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_color_grey() +
  theme_classic()
p

tiff('nets_lat.tif', w=3000, h=6000, units="px", res=600, compression = "lzw")
p
dev.off()

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


###########################
####Biomes
###########################

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

diffs$Biome.name <- factor(diffs$Biome.name, levels = diffs$Biome.name[order(diffs$Freq)])

p <- ggplot(data=diffs, aes(x=Biome.name, y=Freq, fill=Freq)) +
  geom_bar(stat="identity")+
  labs(x="", y="Difference in probability based on biome area") +
  theme_minimal()+
  scale_fill_viridis(option = "D")+
  coord_flip() +
  theme(legend.position="none",
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 16))
p

tiff('prop_biomes.tif', w=9500, h=6000, units="px", res=600, compression = "lzw")
p
dev.off()
###########################
####GDP
###########################

## differences in probabilities for GDP median
gdp.web.dat <- gdp.web.dat[gdp.web.dat$Web.count > 1,]

diffs.gdp <- as.data.frame((gdp.web.dat$Web.count/sum(gdp.web.dat$Web.count) -
                              gdp.web.dat$GDP.MEDIAN/sum(gdp.web.dat$GDP.MEDIAN)))

diffs.gdp$Country <- gdp.web.dat$Country.Code
diffs.gdp$Freq <- diffs.gdp$`(gdp.web.dat$Web.count/sum(gdp.web.dat$Web.count) - gdp.web.dat$GDP.MEDIAN/sum(gdp.web.dat$GDP.MEDIAN))`
diffs.gdp <- diffs.gdp[order(diffs.gdp$Freq),]
diffs.gdp <- diffs.gdp[,-1]

## plot
p <- ggplot(data=diffs.gdp, aes(x=Country, y=Freq)) +
  geom_bar(stat="identity") +
  labs(x="", y="Difference in probability based on GDP")
p + coord_flip()

#making the graph prettier

diffs.gdp$Country <- factor(diffs.gdp$Country, levels = diffs.gdp$Country[order(diffs.gdp$Freq)])

p <- ggplot(data=diffs.gdp, aes(x=Country, y=Freq, fill=Freq)) +
  geom_bar(stat="identity")+
  labs(x="", y="Difference in probability based on GDP") +
  theme_minimal()+
  scale_fill_viridis(option = "D", direction = -1)+
  coord_flip() +
  theme(legend.position="none",
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 16))
p

tiff('prop_gdp.tif', w=6000, h=6000, units="px", res=600, compression = "lzw")
p
dev.off()

###########################
####Research Investment
###########################

## differences in probabilities for research investment
res.inv.web.dat <- res.inv.web.dat[res.inv.web.dat$Web.count > 1,]
diffs.ri <- as.data.frame((res.inv.web.dat$Web.count/sum(res.inv.web.dat$Web.count) -
                          res.inv.web.dat$ResInvestTotal/sum(res.inv.web.dat$ResInvestTotal)))

diffs.ri$Country <- res.inv.web.dat$Country.Code
diffs.ri$Freq <- diffs.ri$`(res.inv.web.dat$Web.count/sum(res.inv.web.dat$Web.count) - res.inv.web.dat$ResInvestTotal/sum(res.inv.web.dat$ResInvestTotal))`
diffs.ri <- diffs.ri[order(diffs.ri$Freq),]
diffs.ri <- diffs.ri[,-1]

#making the graph prettier

diffs.ri$Country <- factor(diffs.ri$Country, levels = diffs.ri$Country[order(diffs.ri$Freq)])

p <- ggplot(data=diffs.ri, aes(x=Country, y=Freq, fill=Freq)) +
  geom_bar(stat="identity")+
  labs(x="", y="Difference in probability based on Research investment") +
  theme_minimal()+
  scale_fill_viridis(option = "D", direction = -1)+
  coord_flip() +
  theme(legend.position="none",
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 16))
p

tiff('prop_res_inv.tif', w=6000, h=6000, units="px", res=600, compression = "lzw")
p
dev.off()

###########################
####Bees diversity
###########################

## differences in probabilities for bees species
country.area.web.dat <- country.area.web.dat[country.area.web.dat$Web.count > 1,]
diffs.bees <- as.data.frame((country.area.web.dat$Web.count/sum(country.area.web.dat$Web.count) -
                             country.area.web.dat$CL_Species/sum(country.area.web.dat$CL_Species)))

diffs.bees$Country <- country.area.web.dat$Country.Code
diffs.bees$Freq <- diffs.bees$`(country.area.web.dat$Web.count/sum(country.area.web.dat$Web.count) - country.area.web.dat$CL_Species/sum(country.area.web.dat$CL_Species))`
diffs.bees <- diffs.bees[order(diffs.bees$Freq),]
diffs.bees <- diffs.bees[,-1]

#making the graph prettier

diffs.bees$Country <- factor(diffs.bees$Country, levels = diffs.bees$Country[order(diffs.bees$Freq)])

p <- ggplot(data=diffs.bees, aes(x=Country, y=Freq, fill=Freq)) +
  geom_bar(stat="identity")+
  labs(x="", y="Difference in probability based on Bees' diversity") +
  theme_minimal()+
  scale_fill_viridis(option = "D", direction = -1)+
  coord_flip() +
  theme(legend.position="none",
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 16))
p

tiff('prop_bees.tif', w=6000, h=6000, units="px", res=600, compression = "lzw")
p
dev.off()


#
# DISTORTED MAPS
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


################
##MODELS GRAPHS
################
library(magrittr)
library(dplyr)
library(purrr)
library(forcats)
library(tidyr)
library(modelr)
library(ggdist)
library(tidybayes)
library(ggplot2)
library(cowplot)
library(rstan)
library(brms)
library(ggrepel)
library(RColorBrewer)
library(gganimate)
library(posterior)
library(ggforce)

theme_set(theme_tidybayes() + panel_border())

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


#graph for biomes
load(file="../network-bias-saved/saved/biomes_data.Rdata")
load(file="../network-bias/saved/biome_mod_negbin.Rdata")

biomes_web_data[biomes_web_data$Hemisphere!='Global',] %>%
  ggplot(aes(y = Webs, x = Area)) +
  geom_point(aes(colour = factor(Hemisphere)), size = 3)

get_variables(biome.mod)

biome_mod_fig <- biomes_web_data[biomes_web_data$Hemisphere!='Global',] %>%
  group_by(Hemisphere) %>%
  data_grid(Area = seq_range (Area, n=26)) %>%
  add_epred_draws(biome.mod) %>%
  ggplot(aes(x = log(Area), y = log(Webs+1), color = ordered(Hemisphere))) +
  stat_lineribbon(aes(y = log(.epred+1))) +
  geom_point(data = biomes_web_data[biomes_web_data$Hemisphere!='Global',]) +
  xlab("Biome area (log)") +
  ylab("Networks (log)") +
  labs(colour="Hemisphere")+
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position="bottom", legend.box = "vertical",
        axis.text=element_text(size=10),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))+
  guides(fill=guide_legend("Confidence level"))

png('model_biome_2.png', w=2000, h=2200, units="px", res=400)
biome_mod_fig
dev.off()

#graph for countries
load(file="../network-bias-saved/saved/webs_all_data.Rdata")
load(file="../network-bias/saved/country_mod_negbin.Rdata")

gdp_area_species %>%
  ggplot(aes(y = Web.count, x = ResInvestTotal)) +
  geom_point(aes(colour = factor(Continent)), size = 2)

get_variables(country.mod)
plot(country.mod)
summary(country.mod)
hist(gdp_area_species$CL_Species)

gdp_area_species <- gdp_area_species[gdp_area_species$CL_Species != max(gdp_area_species$CL_Species, na.rm=TRUE),]

gdp_area_species <- gdp_area_species[gdp_area_species$Continent
                                     != "Oceania",]

gdp_area_species$Continent  <-  factor(gdp_area_species$Continent,
                                       levels=c("North America",
                                                "Asia", "Africa",
                                                "Europe",
                                                "South America"))
summary(gdp_area_species)
####AREA BY COUNTRY

model_country_area <- gdp_area_species %>%
  group_by(Continent) %>%
  data_grid(AREA = seq_range(AREA, n=10), CL_Species = mean(CL_Species),
            ResInvestTotal = mean(ResInvestTotal, na.rm=TRUE)) %>%
  add_epred_draws(country.mod) %>%
  ggplot(aes(x = log(AREA), y = Web.count, color = ordered(Continent))) +
  stat_lineribbon(aes(y = .epred)) +
  geom_point(data = gdp_area_species) +
  xlab("Country area (log)") +
  ylab("Networks") +
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position="bottom", legend.box = "vertical")
  #facet_zoom(xlim = c(14, 16))

tiff('model_country_area.tif', w=2500, h=2200, units="px", res=400, compression = "lzw")
model_country_area
dev.off()

#####BEE SPECIES BY COUNTRY
model_bees <- gdp_area_species %>%
  group_by(Continent) %>%
  data_grid(CL_Species = seq_range(CL_Species, n=10), AREA = mean(AREA),
            ResInvestTotal= mean(ResInvestTotal, na.rm=TRUE)) %>%
  add_epred_draws(country.mod) %>%
  ggplot(aes(x = log(CL_Species), y = Web.count, color = ordered(Continent))) +
  stat_lineribbon(aes(y = .epred)) +
  geom_point(data = gdp_area_species) +
  xlab("Bees' species (log)") +
  ylab("Networks") +
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position="bottom", legend.box = "vertical")

tiff('model_bees.tif', w=2500, h=2200, units="px", res=400, compression = "lzw")
model_bees
dev.off()

#######RESEARCH INVESTMENT BY COUNTRY
res_inv_model <- gdp_area_species %>%
  group_by(Continent) %>%
  data_grid(ResInvestTotal = seq_range(ResInvestTotal, n=10), CL_Species = mean(CL_Species),
            AREA= mean(AREA, na.rm=TRUE)) %>%
  add_epred_draws(country.mod) %>%
  ggplot(aes(x = log(ResInvestTotal), y = Web.count, color = ordered(Continent))) +
  stat_lineribbon(aes(y = .epred)) +
  geom_point(data = gdp_area_species) +
  xlab("Research investment (log)") +
  ylab("Networks") +
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position="bottom", legend.box = "vertical")




