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
area.richness <- read.csv("raw/bees_by_country.csv")
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
  summarise(webs_country_region = n())


# Count occurrences of each unique code by region
code_counts <- table(country.real.data$iso3c, country.real.data$Region)

# Check if any code appears more than once in each region
region_check <- apply(code_counts, 2, function(x) any(x > 1))

# Output warnings for regions where codes appear more than once
if (any(region_check)) {
  warning("Warning: One or more codes appear more than once in the following regions: ", 
          paste(names(region_check)[region_check], collapse = ", "))
} else {
  print("All codes are unique within each region.")
}


country.real.data.country <- webs %>%
  group_by(iso3c)%>%
  summarise(webs_country = n())

## ## all should have a country code now
(country.real.data$iso3c)[!(country.real.data$iso3c) %in% gdp$Country.Code]

complete <- merge(country.real.data, country.real.data.country)

complete <- merge(complete, webs)

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
                        "TLA", "TMM", "TSA", "TSS", "UMC", "PRK",
                        "", "VAT")

dim(gdp)
gdp <- gdp[!gdp$Country.Code %in% not.real.countries,]
dim(gdp)

## **********************************************
#######FROM HERE- what's the point of this

## we don't want to modify the original data because we will join it
## with other data later on with different missing country data

## which countries in the data are not in the gdp
complete[!complete$iso3c %in% gdp$Country.Code,]$iso3c

## **********************************************
## remove countries with NA gdp
dim(gdp)
gdp <- gdp[!is.na(gdp$'X2020'),]
dim(gdp)

## countries with gdp data but no webs
no.webs <- data.frame(iso3c = unique(gdp$Country.Code[!gdp$Country.Code %in% complete$iso3c]))

## this is real data, there are no webs from these countries, so
## create 0 count data and add them to the data
## NA we count as true zeros. These are countries without GDP for usually
## political reasons but are large areas with bees

no.webs$webs_country <- 0

no.webs$webs_country_region <- 0

# Add missing columns to no.webs (fill with NA or a default value)
missing_cols <- setdiff(colnames(complete), colnames(no.webs))

# Add these columns to no.webs with NA (or set a default value if needed)
for (col in missing_cols) {
  no.webs[[col]] <- NA
}

# Ensure the columns are in the same order
no.webs <- no.webs[, colnames(complete)]

# Now use rbind to combine the rows
complete <- rbind(complete, no.webs)

# Find countries in complete$iso3c but not in gdp$Country.Code
missing_in_gdp <- setdiff(complete$iso3c, gdp$Country.Code)

# Print the missing countries
print("Countries in complete but not in gdp:")
print(missing_in_gdp)

## we need to drop the webs that don't report GDP
dim(complete)
complete <- complete[complete$iso3c != missing_in_gdp,]
dim(complete)

## need the gdp to convert proportion to $$
# Select columns for the years 2000 to 2020 using the correct pattern
year_columns <- grep("^X(200[0-9]|201[0-9]|2020)$", names(gdp))

# Calculate the row medians for those columns
gdp$GDP.MEDIAN <- apply(gdp[, year_columns], 1, median, na.rm = TRUE)

# View the result
head(gdp)

#rename iso3c column of df to help with merging
names(complete)[names(complete) == "iso3c"] <- "Country.Code"

complete <- left_join(complete, gdp[, c("Country.Code", "GDP.MEDIAN")], by = join_by(Country.Code))



# Open a new graphics device
dev.new()

# Set up layout with 1 row and 3 columns
layout(matrix(1, ncol = 1))

# Filter out rows with missing or invalid Country.code
filtered_data <- complete %>%
  distinct(Country.Code,  .keep_all = TRUE)

# Plot the histogram for each country based on Country.code
hist(log(filtered_data$GDP.MEDIAN), 
     main="GDP 20 year median", 
     xlab="GDP in US dollars (log)", 
     ylab="Number of countries")

#high and low gdp values
#USA
filtered_data$Country.Code[filtered_data$GDP.MEDIAN == max(filtered_data$GDP.MEDIAN)]
#TUV
filtered_data$Country.Code[filtered_data$GDP.MEDIAN == min(filtered_data$GDP.MEDIAN)]

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

## research $ data but no web data officially collected
## countries with res investment data but no webs
no.res.inv <- res.inv$Country.Code[!res.inv$Country.Code %in% complete[complete$webs >0,]$Country.Code]
no.res.inv

#Countries with no research investments
# Subset rows where Country.Code is not in res.inv, then extract Country.Code
complete[!complete$Country.Code %in% res.inv$Country.Code, ]$Country.Code

#merge gdp_median with complete df
complete <- left_join(complete, res.inv[,c("Country.Code", "PropGDP_median")], by="Country.Code")

## convert proportion to $$ by multiplying gdp and prop
complete$ResInvestTotal <- complete$PropGDP_median*complete$GDP.MEDIAN

## no gdp
print(unique(complete$Country.Code[is.na(complete$GDP.MEDIAN)]))

## drop NAs DO WE WANT TO BE DOING THIS???
dim(complete)
complete <- complete[!is.na(complete$ResInvestTotal),]
dim(complete)

## high and low values of research investment
## USA
unique(complete$Country.Code[complete$ResInvestTotal == max(complete$ResInvestTotal)])

## st. vincent and grenadines
unique(complete$Country.Code[complete$ResInvestTotal == min(complete$ResInvestTotal)])

hist(log(complete$ResInvestTotal),
     main="Research invenstment 20 year median", xlab="Investment in US dollars (log)",
     ylab="Number of countries")


## ***********************************************
## Area and bees' diversity by country
## ***********************************************
## VAT= vatican, FM= micronesia

area.richness <- area.richness[, c("NAME", "ISO3", "AREA", "CL_Species")]
names(area.richness)[names(area.richness) == "ISO3"] <- "Country.Code"

area.richness <- area.richness[!area.richness$Country.Code %in% not.real.countries,]

#counties with area richness in our data set
length(area.richness[area.richness$Country.Code %in% unique(complete$Country.Code),]$Country.Code)
#counties with area richness not in our data set
length(area.richness[!area.richness$Country.Code %in% unique(complete$Country.Code),]$Country.Code)

## drop really small islands that are territories and would have been
## coded as part of the colonial empire
area.richness$AREA <- as.numeric(area.richness$AREA)
area.richness <- area.richness[area.richness$AREA > 1000,]

## drops countries with no recorded bees
area.richness <-  area.richness[!is.na(area.richness$CL_Species),]

## drop islands off of Antarctica/ Madagascar because they are combined
## but each is less than 1000 km
area.richness <-  area.richness[area.richness$Country.Code != "ATF",]
area.richness <-  area.richness[area.richness$Country.Code != "MDG",]
## drop Western Sahara "disputed territory"
area.richness <-  area.richness[area.richness$Country.Code != "ESH",]

## merging networks and bee richness by country
country.area.web.dat <- left_join(complete,
                              area.richness,
                              by = "Country.Code")

## ***********************************************
## Biomes
## ***********************************************

## load biome data
biomes <- st_read(dsn="official", layer="wwf_terr_ecos")
# Standardize biome names (uppercase and remove commas)
biome.code$BiomeName <- toupper(gsub(",", "", biome.code$BiomeName))
webs$Biome_WWF <- toupper(gsub(",", "", webs$Biome_WWF))

# Correct biome name issues
biome.code$BiomeName <- gsub("SCRUB", "SHRUB", biome.code$BiomeName)
biome.code$BiomeName <- gsub("TEMPERATE CONIFER FORESTS", "TEMPERATE CONIFEROUS FORESTS", biome.code$BiomeName)
biome.code$BiomeName <- gsub("MANGROVES", "MANGROVE", biome.code$BiomeName)

# Handle missing values in `Biome_WWF`
webs$Biome_WWF[webs$Biome_WWF == "#N/A"] <- NA

# Find unmatched biome names
unmatched_biomes <- setdiff(unique(webs$Biome_WWF), biome.code$BiomeName)
unmatched_biomes

# Match Biome_WWF to BiomeCode
webs$BiomeCode <- biome.code$BIOME[match(webs$Biome_WWF, biome.code$BiomeName)]

## sum the area for each biome in the S, N and the entire globe
## drop codes that are for missing data
biomes <- biomes[!biomes$BIOME %in% c(98,99),]

# Ensure all geometries are valid
biomes <- biomes %>%
  mutate(geometry = st_make_valid(geometry))

# Extract centroid latitudes for each geometry
biomes <- biomes %>%
  mutate(
    hemisphere = ifelse(st_coordinates(st_centroid(geometry))[, 2] >= 0, 
                        "Northern", 
                        "Southern")
  )

# Summarize the data
biome_area_summary <- biomes %>%
  group_by(BIOME) %>%
  summarize(
    total_area_north = sum(AREA[hemisphere == "Northern"], na.rm = TRUE),
    total_area_south = sum(AREA[hemisphere == "Southern"], na.rm = TRUE),
    total_area_global = sum(AREA, na.rm = TRUE)
  )

biome_area_summary <- biome_area_summary %>%
  st_drop_geometry()

#these zeros were manually added in as part of the old pipeline
#Is there a biologically relevant reason why this happened?
#Some of these areas are not actually zero when I use a pipe to combine area
# southern.real.dat <- c(southern.real.dat,
#                        "14"=0, "11"=0, "3"=0)
# northern.real.dat <- c(northern.real.dat,
#                        "9"=0)

webs_biome <- webs %>%
  filter(!is.na(BiomeCode)) %>%  # Exclude rows with missing BiomeCode
  group_by(BiomeCode) %>%
  summarise(
    NorthernWebs = sum(Hemisphere == "Northern", na.rm = TRUE),
    SouthernWebs = sum(Hemisphere == "Southern", na.rm = TRUE),
    total_webs_global = n()
  ) %>%
  left_join(select(webs, BiomeCode, Biome_WWF)%>% distinct(), by = "BiomeCode")

biome_area_summary <- merge(webs_biome, biome_area_summary)

save(biome_area_summary, global.real.dat, northern.real.dat, globe.area.biome,
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

