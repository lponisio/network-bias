
## ***********************************************
library(tidyverse)
library(sf)
library(dplyr)
library(countrycode)

source("~/lab_paths.R")
setwd(local.path)

#get data
setwd("network-bias-saved/")
webs <- read.csv("raw/webs.csv", sep = ";")
res.inv <- read.csv("raw/research_expenditure.csv")
gdp <- read.csv("raw/gdp.csv")
biome.code <- read.csv("raw/biome_codes.csv")
area.richness <- read.csv("raw/bees_by_country.csv")


## load biome data
biomes <- st_read(dsn="official", layer="wwf_terr_ecos")








# ## ***********************************************
# # Count occurrences of each unique code by region
# code_counts <- table(country.real.data$iso3c, country.real.data$Region)
# 
# # Check if any code appears more than once in each region
# region_check <- apply(code_counts, 2, function(x) any(x > 1))
# 
# # Output warnings for regions where codes appear more than once
# if (any(region_check)) {
#   warning("Warning: One or more codes appear more than once in the following regions: ", 
#           paste(names(region_check)[region_check], collapse = ", "))
# } else {
#   print("All codes are unique within each region.")
# }
