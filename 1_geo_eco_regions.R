rm(list=ls())

## ***********************************************
source("~/lab_paths.R")
setwd(local.path)
source("network-bias/src/initailize.R")

## ***********************************************
## General cleaning of web data
## ***********************************************

webs_reuse_count <- webs_reuse%>%
  group_by(Web_Code)%>%
  summarise(webs_reuse_count = n()-1)


webs_reuse_count

#webs <- merge(webs_reuse_count, webs)
webs <- left_join(webs, webs_reuse_count, by = "Web_Code")

#saving and sending to E
x <-webs[is.na(webs$webs_reuse_count),]

write.csv(x, file = "cleaning/webs_missing_new_reuse.csv")


#around 70 networks that were in the previous webs_reuse csv are not in the updated one
#this is to carry over that column
webs <- webs %>%
  mutate(webs_reuse_count = if_else(is.na(webs_reuse_count), Use_Frequency - 1, webs_reuse_count))


## ***********************************************

#columns to keep 
col_keep <- c("Web_Code", "webs_reuse_count", "Publi_Year","LAT", "LONG", "Region",
              "Country", "ISO3", "Continent", "Hemisphere","Biome_WWF")

# Keep only the specified columns
webs <- webs[, col_keep, drop = FALSE]

# Standardize and validate country names
length(unique(webs$Country))

webs$Country <- countrycode(as.character(webs$Country), 
                                         origin = "country.name", destination = "country.name")
length(unique(webs$Country))

#making sure the ISO3 is standardized
webs$ISO3 <- countrycode(as.character(webs$Country), 
                          origin = "country.name", destination = "iso3c")

## ## webs without countries, nothing we can do here
dim(webs)
webs <- webs %>% filter(!is.na(Country) & Country != "",
                        !is.na(ISO3) & ISO3 != "")
dim(webs)


## ***********************************************
#really cool way to handle colonies or sovereignt states!
#I added a new column, one for the sovereignt (iso3c)or the colonial country
#and one for the colony (admin or adm0_a3)

library(rnaturalearth)
library(dplyr)

# Load countries with sovereignty info
countries <- rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_drop_geometry() %>%
  dplyr::select(sovereignt, admin,  adm0_a3)%>%
  mutate(
    ISO3 = countrycode(sovereignt, origin = 'country.name', destination = 'iso3c'))
  
  
names(webs)[names(webs) == "ISO3"] <- "adm0_a3"

final <- left_join(webs, countries, by = "adm0_a3")




## ***********************************************
#count up the webs in each Country
web_country <- webs %>%
  group_by(adm0_a3)%>%
  summarise(Total_webs_by_country = n())

## ## all should have a country code now
(final$adm0_a3)[!(final$adm0_a3) %in% gdp$Country.Code]

#this is so you can just subset the original dataframe later on
final <- left_join(web_country, final, by = "adm0_a3")


#new column for economic IS03


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




## need the gdp to convert proportion to $$
# Select columns for the years 2000 to 2020 using the correct pattern
year_columns <- grep("^X(200[0-9]|201[0-9]|2023)$", names(gdp))

# Calculate the row medians for those columns
gdp$GDP.MEDIAN <- apply(gdp[, year_columns], 1, median, na.rm = TRUE)


## remove countries with NA gdp
dim(gdp)
gdp <- gdp[!is.na(gdp$GDP.MEDIAN),]
dim(gdp)


# Handle missing values in `Biome_WWF`
final$Biome_WWF[final$Biome_WWF == "#N/A"] <- NA
final <- final[!is.na(final$Biome_WWF),]

## countries with gdp data but no webs
no.webs <- data.frame(ISO3 = unique(gdp$Country.Code[!gdp$Country.Code %in% final$ISO3]))



## this is real data, there are no webs from these countries, so
## create 0 count data and add them to the data
## NA we count as true zeros. These are countries without GDP for usually
## political reasons but are large areas with bees
no.webs$Total_webs_by_country <- 0

# Add missing columns to no.webs (fill with NA or a default value)
missing_cols <- setdiff(colnames(final), colnames(no.webs))

# Add these columns to no.webs with NA (or set a default value if needed)
for (col in missing_cols) {
  no.webs[[col]] <- NA
}

# Ensure the columns are in the same order
no.webs <- no.webs[, colnames(final)]

# Now use rbind to combine the rows
final <- rbind(final, no.webs)


final <- final %>%
  mutate(adm0_a3 = if_else(is.na(adm0_a3), ISO3, adm0_a3))

## ***********************************************
#Assing contintents to countries where the data is missing
final$Continent <- countrycode(sourcevar = final$ISO3, 
                                      origin = "iso3c", 
                                      destination = "continent")

#it can't recgonize the continent for these
final[final$ISO3 == "TMN",]$Continent == "Asia"
final[final$ISO3 == "XKX",]$Continent == "Europe"

## ***********************************************
#assigning hemisphere to countries with no webs, therefore no assigned hemisphere
#Get world map data
world_map <- map_data("world")

# Extract country names and latitudes
latitudes <- world_map %>%
  group_by(region) %>%
  summarize(LAT = mean(lat, na.rm = TRUE))

# Match country names to ISO3 codes
latitudes$adm0_a3 <- countrycode(latitudes$region, origin = "country.name", destination = "iso3c")

# Drop NA values (countries that couldn't be matched)
latitudes <- latitudes[!is.na(latitudes$adm0_a3), ]

# Merge with your dataset
final <- left_join(final, latitudes, by = "adm0_a3")

## ***********************************************
# Assign hemisphere
final <- final %>%
  mutate(Hemisphere = ifelse(is.na(Hemisphere), ifelse(LAT.y >= 0, "Northern", "Southern"), Hemisphere))

# Check if any missing hemispheres remain
sum(is.na(final$Hemisphere))

## ***********************************************
#Assigning north and south america 
final <- final %>%
  mutate(Continent = ifelse(Continent == "Americas" & Hemisphere == "Northern", 
                            "North America", 
                            ifelse(Continent == "Americas" & Hemisphere == "Southern", 
                                   "South America", 
                                   Continent)))

## ***********************************************
# I disagree with this, I think we should keep VEN
# # Find countries in complete$iso3c but not in gdp$Country.Code
# missing_in_gdp <- setdiff(final$ISO3, gdp$Country.Code)
# 
# # Print the missing countries
# print("Countries in complete but not in gdp:")
# print(missing_in_gdp)
# 
# ## we need to drop the webs that don't report GDP
# dim(final)
# final <- final[final$ISO3 != missing_in_gdp,]
# dim(final)



# View the result
head(gdp)

#rename iso3c column of df to help with merging
names(gdp)[names(gdp) == "Country.Code"] <- "ISO3"

final <- left_join(final, gdp[, c("ISO3", "GDP.MEDIAN")], by = join_by(ISO3))

final <- final %>%
  mutate(ISO3 = if_else(is.na(ISO3), ISO3, ISO3))

#high and low gdp values
#USA
unique(final$ISO3[final$GDP.MEDIAN == max(final$GDP.MEDIAN)])
#TUV
unique(final$ISO3[final$GDP.MEDIAN == min(final$GDP.MEDIAN)])

## ***********************************************
## research investment by country
## ***********************************************
dim(res.inv)
res.inv <- res.inv[!res.inv$Country.Code %in% not.real.countries,]
dim(res.inv)

## take the 20 year median
year_columns <- grep("^X(200[0-9]|201[0-9]|2023)$", names(res.inv))

res.inv$PropGDP_median <- apply(res.inv[, year_columns], 1, median, na.rm = TRUE)


## research $ data but no web data officially collected
## countries with res investment data but no webs
res.inv.no.web <- res.inv$Country.Code[res.inv$Country.Code %in% 
                                    final[final$Total_webs_by_country ==0,]$ISO3]
res.inv.no.web

#Countries with no research investments
# Subset rows where Country.Code is not in res.inv, then extract Country.Code
no.res.inv.web <- final$ISO3[!final$ISO3 %in% res.inv$Country.Code]
res.inv.no.web

#rename iso3c column of df to help with merging
names(res.inv)[names(res.inv) == "Country.Code"] <- "ISO3"

#merge gdp_median with complete df
final <- left_join(final, res.inv[,c("ISO3", "PropGDP_median")], by="ISO3")

## convert proportion to $$ by multiplying gdp and prop
final$ResInvestTotal <- final$PropGDP_median*final$GDP.MEDIAN

## no gdp
print(unique(final$ISO3[is.na(final$GDP.MEDIAN)]))

## drop NAs DO WE WANT TO BE DOING THIS???
dim(final)
final <- final[!is.na(final$ResInvestTotal),]
dim(final)

## high and low values of research investment
## USA
unique(final$ISO3[final$ResInvestTotal == max(final$ResInvestTotal)])

## st. vincent and grenadines
unique(final$ISO3[final$ResInvestTotal == min(final$ResInvestTotal)])

hist(log(final$ResInvestTotal),
     main="Research invenstment 20 year median", xlab="Investment in US dollars (log)",
     ylab="Number of countries")


## ***********************************************
## Area and bees' diversity by country
## ***********************************************
## VAT= vatican, FM= micronesia
area.richness <- area.richness[, c("NAME", "ISO3", "AREA", "CL_Species")]

#switching the colonies back 

dim(area.richness)
area.richness <- area.richness[!area.richness$ISO3 %in% not.real.countries,]
dim(area.richness)

#counties with area richness in our data set
length(area.richness[area.richness$NAME %in% final$adm0_a3,]$ISO3)

#counties with area richness not in our data set
length(unique(area.richness[!area.richness$NAME  %in% final$adm0_a3,]$ISO3))
####DOUBLE CHECK
#does dropping 99 make sense?


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


#since this is by country not colonial power
names(area.richness)[names(area.richness) == "ISO3"] <- "adm0_a3"


## merging networks and bee richness by country
final <- left_join(final, area.richness, by = "adm0_a3")

## ***********************************************
## Biomes
## ***********************************************
# 
# # Standardize biome names (uppercase and remove commas)
# biome.code$BiomeName <- toupper(gsub(",", "", biome.code$BiomeName))
# final$Biome_WWF <- toupper(gsub(",", "", final$Biome_WWF))
# 
# # Correct biome name issues
# biome.code$BiomeName <- gsub("SCRUB", "SHRUB", biome.code$BiomeName)
# biome.code$BiomeName <- gsub("TEMPERATE CONIFER FORESTS", "TEMPERATE CONIFEROUS FORESTS", biome.code$BiomeName)
# biome.code$BiomeName <- gsub("MANGROVES", "MANGROVE", biome.code$BiomeName)
# 
# # Handle missing values in `Biome_WWF`
# final$Biome_WWF[final$Biome_WWF == "#N/A"] <- NA
# 
# # Find unmatched biome names
# unmatched_biomes <- setdiff(unique(final$Biome_WWF), biome.code$BiomeName)
# unmatched_biomes
# 
# # Match Biome_WWF to BiomeCode
# final$BiomeCode <- biome.code$BIOME[match(final$Biome_WWF, biome.code$BiomeName)]
# 
# ## drop codes that are for missing data
# biomes <- biomes[biomes$BIOME %in% biome.code$BIOME,]
# 
# # Ensure all geometries are valid
# biomes <- biomes %>%
#   mutate(geometry = st_make_valid(geometry))
# 
# # Extract centroid latitudes for each geometry
# biomes <- biomes %>%
#   mutate(
#     hemisphere = ifelse(st_coordinates(st_centroid(geometry))[, 2] >= 0, 
#                         "Northern", 
#                         "Southern")
#     )
# 
# # Summarize the data
# biome_area_summary <- biomes %>%
#   group_by(BIOME) %>%
#   summarize(
#     AREA_biome_north = sum(AREA[hemisphere == "Northern"], na.rm = TRUE),
#     AREA_biome_south = sum(AREA[hemisphere == "Southern"], na.rm = TRUE),
#     AREA_biome_total = sum(AREA, na.rm = TRUE), #biome,
#   )%>%
#   st_drop_geometry()
# 
# 
# names(biome_area_summary)[names(biome_area_summary) == "BIOME"] <- "BiomeCode"
# 
# final <- left_join(final, biome_area_summary,by ="BiomeCode")
# 
# final <- final %>%
#   mutate(area_by_hemisphere = case_when(
#     Hemisphere == "Northern" ~ AREA_biome_north,
#     Hemisphere == "Southern" ~ AREA_biome_south,
#     TRUE ~ NA_real_  # Handle any unexpected cases
#   )) %>%
#   dplyr::select(-AREA_biome_north, -AREA_biome_south)
# 
# # Summarize the data
# webs_biome_hemi <- final %>%
#   filter(BiomeCode != "NA")%>%
#   group_by(BiomeCode, Hemisphere) %>%
#   summarize(Total_webs_by_biome_by_hemi = n()
#   )
# 
# final <- left_join(final, webs_biome_hemi,by = c("BiomeCode", "Hemisphere"))

## ***********************************************
## Years since published
## ***********************************************
final <- final %>%
  mutate(years_since_pub = as.numeric(format(Sys.Date(), "%Y")) - Publi_Year)


## ***********************************************
## Correcting Continent
## ***********************************************

final <- final %>%
  mutate(Continent = ifelse(Continent == "Americas" & Hemisphere == "Northern", 
                            "North America", 
                            ifelse(Continent == "Americas" & Hemisphere == "Southern", 
                                   "South America", 
                                   Continent)))

## ***********************************************
## Density
## ***********************************************
#country level
final$CL_Species_Density <- final$CL_Species/ final$AREA

#sovergein level
densities <- final %>%
   distinct(adm0_a3, .keep_all = TRUE) %>%
   group_by(ISO3) %>%
   summarize(AREA_by_ISO3 =sum(AREA))

final <- left_join(final, densities, by = "ISO3")


final$ResInvs_Density <- final$ResInvestTotal/ final$AREA_by_ISO3

  
  
## ***********************************************
write.csv(final, file = "saved/webs_complete.csv")







