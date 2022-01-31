library(sf)             # spatial data classes
library(rnaturalearth)  # world map data
library(readxl)         # reading excel files
library(dplyr)          # data manipulation
library(tidyr)          # data manipulation
library(purrr)          # data manipulation
library(cartogram)      # cartograms creation
library(tmap)           # maps creation

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
