rm(list=ls())
library(tidyverse)
#library(rgdal) RETIRED!
library(sf)
#library(maptools)RETIRED!
library(dplyr)
library(countrycode)

## ***********************************************
source("~/lab_paths.R")
local.path

## ***********************************************
## General cleaning of web data
## ***********************************************
setwd(paste0(local.path, "network-bias-saved/"))
webs <- read.csv("raw/network_papers_2021_2.csv",  sep=";")
res.inv <- read.csv("raw/research_expenditure.csv")
gdp <- read.csv("raw/gdp.csv")
biome.code <- read.csv("raw/biome_codes.csv")
countries <- read.csv("raw/bees_by_country.csv")
## ***********************************************

#dropping any rows with no country information
dim(webs)
webs <-webs[!is.na(webs$Country),]
dim(webs)


# Standardize and validate country names
webs$country_standardized <- countrycode(as.character(webs$Country), origin = "country.name", destination = "country.name")
webs$iso3c <- countrycode(as.character(webs$country_standardized), origin = "country.name", destination = "iso3c")


# Identify entries that were not matched to standard country names
invalid_entries <- webs[is.na(webs$country_standardized), ]
valid_data <- webs[!is.na(webs$country_standardized), ]

# Output results
print("Invalid entries:")
print(invalid_entries$iso3c)

## ## webs without countries, nothing we can do here
webs <- webs %>% filter(!is.na(country_standardized) & country_standardized != "")

#This was at a lower step in the pipeline but I moved it up to avoid errors
## because Greenland is a territory of Denmark, the gdp we would like
## to consider is the Danish GDP
webs[webs$iso3c == "GRL",]$iso3c <- "DNK"


#count up the webs in each Country
country.real.data <- webs %>%
  group_by(iso3c, Region)%>%
  summarise(webs = n())

country.real.data.country <- webs %>%
  group_by(iso3c)%>%
  summarise(webs = n())

## ## all should have a country code now
(country.real.data$iso3c)[!(country.real.data$iso3c) %in% gdp$Country.Code]


#countries <- countries[, c("NAME", "ISO3", "AREA", "CL_Species")]

#Why save at this step?
#save(webs, res.inv, gdp, biome.code, countries, country.real.data,
#      file="../network-bias/data/rawdata.rdata")

## ***********************************************
## join the web data with the gdp data
## clean gdp data
## **********************************************

# drop the codes for regions, and country groupings
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

dim(gdp)
gdp <- gdp[!gdp$Country.Code %in% not.real.countries,]
dim(gdp)

## **********************************************
#######FROM HERE- what's the point of this

## we don't want to modify the original data because we will join it
## with other data later on with different missing country data

## which countries in the data are not in the gdp
country.real.data[!country.real.data$iso3c %in% gdp$Country.Code,]$iso3c

## **********************************************
## remove countries with NA gdp
dim(gdp)
gdp <- gdp[!is.na(gdp$'X2020'),]
dim(gdp)

## countries with gdp data but no webs
no.webs <- data.frame(iso3c = gdp$Country.Code[!gdp$Country.Code %in% country.real.data$iso3c])

## this is real data, there are no webs from these countries, so
## create 0 count data and add them to the data
no.webs$webs <- 0

country.real.dat.gdp <- rbind(country.real.data, no.webs)

# Count occurrences of each unique code by region
code_counts <- table(country.real.dat.gdp$iso3c, country.real.dat.gdp$Region)

# Check if any code appears more than once in each region
region_check <- apply(code_counts, 2, function(x) any(x > 1))

# Output warnings for regions where codes appear more than once
if (any(region_check)) {
  warning("Warning: One or more codes appear more than once in the following regions: ", 
          paste(names(region_check)[region_check], collapse = ", "))
} else {
  print("All codes are unique within each region.")
}

# Countries in gdp but not in country.real.dat.gdp
missing_in_real <- setdiff(gdp$Country.Code, country.real.dat.gdp$iso3c)
print(missing_in_real)

# Countries in country.real.dat.gdp but not in gdp
missing_in_gdp <- setdiff(country.real.dat.gdp$iso3c, gdp$Country.Code)
print(missing_in_gdp)

## we need to drop the webs that don't report GDP
dim(country.real.dat.gdp)
country.real.dat.gdp <- country.real.dat.gdp[country.real.dat.gdp$iso3c != missing_in_gdp,]
dim(country.real.dat.gdp)

## need the gdp to convert proportion to $$
# Select columns for the years 2000 to 2020 using the correct pattern
year_columns <- grep("^X(200[0-9]|201[0-9]|2020)$", names(gdp))

# Calculate the row medians for those columns
gdp$GDP.MEDIAN <- apply(gdp[, year_columns], 1, median, na.rm = TRUE)

# View the result
head(gdp)

## subset to median
gdp.median <- gdp[, c("Country.Code", "GDP.MEDIAN")]

#rename iso3c column of df to help with merging
names(country.real.dat.gdp)[names(country.real.dat.gdp) == "iso3c"] <- "Country.Code"

gdp.web.dat <- left_join(country.real.dat.gdp, gdp.median, by = join_by(Country.Code))

head(gdp.web.dat)


save(webs,
     file="raw/saved/webs_raw.Rdata")
save(gdp.web.dat,
     file="raw/saved/GDP_web.Rdata")

write.csv(webs, file="raw/cleaned_web_data.csv",
          row.names=FALSE)


# Open a new graphics device
dev.new()

# Set up layout with 1 row and 3 columns
layout(matrix(1, ncol = 1))

# Plot the histogram in the first position (1st of 3 columns)
hist(log(gdp.web.dat$GDP.MEDIAN), main="GDP 20 year median", 
     xlab="GDP in US dollars (log)", ylab="Number of countries")

#high and low gdp values
#USA
gdp.web.dat$Country.Code[gdp.web.dat$GDP.MEDIAN == max(gdp.web.dat$GDP.MEDIAN)]
#TUV
gdp.web.dat$Country.Code[gdp.web.dat$GDP.MEDIAN == min(gdp.web.dat$GDP.MEDIAN)]

## (YAY)

## ***********************************************
## research investment by country
## ***********************************************
dim(res.inv)
res.inv <- res.inv[!res.inv$Country.Code %in%  not.real.countries,]
dim(res.inv)

## take the 20 year median
year_columns <- grep("^X(200[0-9]|201[0-9]|2020)$", names(res.inv))

res.inv$PropGDP_median <- apply(res.inv[, year_columns], 1, median, na.rm = TRUE)

res.inv <- merge(res.inv, gdp.median, by="Country.Code")
## convert proportion to $$ by multiplying gdp and prop
res.inv$ResInvestTotal <- res.inv$PropGDP_median*res.inv$GDP.MEDIAN

## no gdp
print(res.inv$Country.Code[is.na(res.inv$GDP.MEDIAN)])

## drop NAs
dim(res.inv)
res.inv <- res.inv[!is.na(res.inv$ResInvestTotal),]
dim(res.inv)
## high and low values of research investment
## USA
res.inv$Country.Code[res.inv$ResInvestTotal == max(res.inv$ResInvestTotal)]

## st. vincent and grenadines
res.inv$Country.Code[res.inv$ResInvestTotal == min(res.inv$ResInvestTotal)]

## countries in the research $ data that we have in the studies dataset
res.inv$Country.Code[res.inv$Country.Code %in% country.real.dat.gdp$Country.Code]

# Find Country Codes in res.inv that are NOT in country.real.dat.gdp
no.webs <- res.inv$Country.Code[!res.inv$Country.Code %in% country.real.dat.gdp$Country.Code]
no.webs
## since we already fixed this with gdp we are good

## research $ data but no web data officially collected
## countries with res investment data but no webs
no.res.inv <- res.inv$Country.Code[!res.inv$Country.Code %in% country.real.dat.gdp[country.real.dat.gdp$webs >0,]$Country.Code]
no.res.inv

#Countries with no research investments
# Subset rows where Country.Code is not in res.inv, then extract Country.Code
country.real.dat.gdp[!country.real.dat.gdp$Country.Code %in% res.inv$Country.Code, ]$Country.Code

## countries with no research investment data but webs
# Subset rows where Country.Code is not in res.inv and webs > 0, then extract Country.Code
no.res.inv <- country.real.dat.gdp[!country.real.dat.gdp$Country.Code 
                                   %in% res.inv$Country.Code 
                                   & country.real.dat.gdp$webs > 0, "Country.Code"]
no.res.inv

#countries with research investment
country.real.dat.res.inv <-
  country.real.dat.gdp[!country.real.dat.gdp$Country.Code %in% no.res.inv$Country.Code,]

#THIS SECTION MAKES NO SENSE TO ME
# ######
# #countries with research investment
# country.real.dat.res.inv <-
#   country.real.dat.gdp[!country.real.dat.gdp$Country.Code %in% no.res.inv$Country.Code,]
# 
# ## subset to country and research investment data
 res.inv.median <- res.inv[, c("Country.Code", "ResInvestTotal")]
# 
# ## alphabetize names
# res.inv.median  <- res.inv.median[order(res.inv.median$Country.Code),]
# 
# country.real.dat.res.inv <- country.real.dat.res.inv[order(
#   names(country.real.dat.res.inv))]
# 
# country.real.dat.res.inv$Country.Code %in% res.inv.median$Country.Code
# ## (YAY) all match
# 
# res.inv.web.dat <- data.frame(
#     "Country.Code" =country.real.dat.res.inv$Country.Code,
#     "Web.count" = country.real.dat.res.inv$webs)
# rownames(res.inv.web.dat) <- NULL

res.inv.web.dat  <- merge(country.real.dat.res.inv, res.inv.median, by="Country.Code")
head(res.inv.web.dat)

hist(log(res.inv.web.dat$ResInvestTotal),
     main="Research invenstment 20 year median", xlab="Investment in US dollars (log)",
     ylab="Number of countries")

save(res.inv.web.dat,
     file="raw/saved/res_inv_web_REGIONS.Rdata")

# Calculate cumulative sum of webs and drop Region, replacing webs with cum_webs
res.inv.web.dat.country <- res.inv.web.dat %>%
  group_by(Country.Code) %>%
  mutate(webs = cumsum(webs)) %>%
  ungroup() %>%
  select(-Region)  # Drop the Region column

# View the updated dataset
head(res.inv.web.dat.country)

save(res.inv.web.dat.country,
     file="raw/saved/res_inv_web_COUNTRY.Rdata")

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
length(area.richness[area.richness$ISO3 %in% gdp$Country.Code,]$ISO3)

## not in gpd
length(area.richness[!area.richness$ISO3 %in% gdp$Country.Code,]$ISO3)

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

names(country.real.data.country)[names(country.real.data.country) == "iso3c"] <- "Country.Code"
names(area.richness)[names(area.richness) == "ISO3"] <- "Country.Code"

## merging networks and bee richness by country
country.area.web.dat <- merge(country.real.data.country,
                              area.richness,
                              by = "Country.Code",
                              all = F)

## NA we count as true zeros. These are countries without GDP for usually
## political reasons but are large areas with bees
country.area.web.dat$webs[is.na(country.area.web.dat$webs)]  <- 0

## no area data but yes gdp data
country.area.web.dat[is.na(country.area.web.dat$NAME),]

save(country.area.web.dat,
     file="raw/saved/area_richness_web.Rdata")

## ***********************************************
## Biomes
## ***********************************************

## load biome data
biomes <- st_read(dsn="official", layer="wwf_terr_ecos")

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
biomes <- biomes[!biomes$BIOME %in% c(98,99),]

## sum the area in each biome
plot(biomes)
southern <- biomes[biomes$geometry[,2] < 0,]
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
     file="raw/saved/biome_webs.Rdata")



############
#adding region variable

gdp_area_species$Region <- webs$Region[match(
  gdp_area_species$Country.Code,
  webs$ISO3)]

write.csv(gdp_area_species, file="raw/gdp_area_species_region.csv",
          row.names=FALSE)

gdp_area_species <- read.csv("gdp_area_species_region.csv",  sep=";")

save(gdp_area_species,
     file="../network-bias-saved/saved/gdp_area_species_region.rdata")

