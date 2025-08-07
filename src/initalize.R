
## ***********************************************
library(tidyverse)
library(sf)
library(countrycode)
library(rnaturalearth)

## Load data
webs <- read.csv("data/raw/webs.csv", sep = ";")
webs_reuse <- read.csv("data/raw/network_reuse.csv", sep = ";")
res.inv <- read.csv("data/raw/research_expenditure.csv")
gdp <- read.csv("data/raw/gdp.csv")
area.richness <- read.csv("data/raw/bees_by_country.csv")
continents <- read.csv("data/raw/UNSD_continents.csv", sep = ";")
