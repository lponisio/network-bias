
## ***********************************************
library(tidyverse)
library(sf)
library(countrycode)
library(rnaturalearth)

## Load data
setwd("network-bias-saved/")
webs <- read.csv("raw/webs.csv", sep = ";")
webs_reuse <- read.csv("raw/network_reuse.csv", sep = ";")
res.inv <- read.csv("raw/research_expenditure.csv")
gdp <- read.csv("raw/gdp.csv")
area.richness <- read.csv("raw/bees_by_country.csv")
continents <- read.csv("raw/UNSD_continents.csv", sep = ";")
