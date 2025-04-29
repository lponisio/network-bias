rm(list=ls())

## ***********************************************
source("~/lab_paths.R")
setwd(local.path)
source("network-bias/src/initailize.R")

library(rnaturalearth)
library(dplyr)


## ***********************************************
## General cleaning of web data
## ***********************************************

webs_reuse_count <- webs_reuse%>%
  group_by(Web_Code)%>%
  summarise(webs_reuse_count = n()-1)

#webs <- merge(webs_reuse_count, webs)
webs <- left_join(webs, webs_reuse_count, by = "Web_Code")


## ***********************************************
#columns to keep 
col_keep <- c("Web_Code", "webs_reuse_count", "Publi_Year","LAT", "LONG", "Region",
              "Continent", "ISO3", "Hemisphere", "Country")

# Keep only the specified columns
webs <- webs[, col_keep, drop = FALSE]

# Standardize and validate country names
x <-unique(webs$Country)

webs$Country <- countrycode(as.character(webs$Country), 
                                         origin = "country.name", destination = "country.name")
y<- unique(webs$Country)

setdiff(x,y)
setdiff(y,x)


#making sure the ISO3 is standardized
x <-unique(webs$ISO3)
webs$ISO3 <- countrycode(as.character(webs$Country), 
                          origin = "country.name", destination = "iso3c")

y<- unique(webs$ISO3)

setdiff(x,y)
setdiff(y,x)


## ## webs without countries, nothing we can do here
dim(webs)
webs <- webs %>% filter(!is.na(Country) & Country != "",
                        !is.na(ISO3) & ISO3 != "")
dim(webs)

countries.with.webs <-unique(webs$ISO3)

## ***********************************************
#count up the webs in each Country
## ***********************************************
#count up the webs in each Country
web_country <- webs %>%
  group_by(ISO3)%>%
  summarise(Total_webs_by_country = n())


#this is so you can just subset the original dataframe later on
webs <- left_join(web_country, webs, by = "ISO3")

## ***********************************************
#rename iso3c column of df to help with merging
names(gdp)[names(gdp) == "Country.Code"] <- "ISO3"
names(gdp)[names(gdp) == "Country.Name"] <- "Country"

## need the gdp to convert proportion to $$
# Select columns for the years 2000 to 2023 using the correct pattern
year_columns <- grep("^X(200[0-9]|201[0-9]|2023)$", names(gdp))

# Calculate the row medians for those columns
gdp$GDP.MEDIAN <- apply(gdp[, year_columns], 1, median, na.rm = FALSE)

## remove countries with NA 20 year gdp
dim(gdp)
gdp <- gdp[!is.na(gdp$GDP.MEDIAN),]
dim(gdp)

countries.no.webs <- gdp[!gdp$ISO3 %in% countries.with.webs, c("ISO3","Country")]


## this is real data, there are no webs from these countries, so
## create 0 count data and add them to the data
## NA we count as true zeros. These are countries without GDP for usually
## political reasons but are large areas with bees
countries.no.webs$Total_webs_by_country <- 0

# Add missing columns to no.webs (fill with NA or a default value)
missing_cols <- setdiff(colnames(webs), colnames(countries.no.webs))

# Add these columns to no.webs with NA (or set a default value if needed)
for (col in missing_cols) {
  countries.no.webs[[col]] <- NA
}

# Ensure the columns are in the same order
countries.no.webs <- countries.no.webs[, colnames(webs)]

# Now use rbind to combine the rows
final <- rbind(webs, countries.no.webs)


## ***********************************************

#really cool way to handle colonies or sovereignt states!
#I added a new column, one for the sovereignt (iso3c)or the colonial country
#and one for the colony (admin or adm0_a3)

# Load countries with sovereignty info
countries <- rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_drop_geometry() %>%
  dplyr::select(sovereignt, admin,  adm0_a3, continent, type)%>%
  mutate(
    ISO3 = countrycode(sovereignt, origin = 'country.name', destination = 'iso3c'))


names(final)[names(final) == "ISO3"] <- "adm0_a3"
names(countries)[names(countries) == "continent"] <- "Continent"


final <- left_join(final, countries, by = "adm0_a3")


## ***********************************************

final <- left_join(final, gdp[, c("ISO3", "GDP.MEDIAN")], by = join_by(ISO3))

hist(final$GDP.MEDIAN)

#not offically recgonized ISO3
no.ISO3 <- final[is.na(final$ISO3),]

write.csv(no.ISO3, file = "cleaning/potentially_drop_20yearGDP.csv")



## ***********************************************
#assigning hemisphere to countries with no webs, therefore no assigned hemisphere
#Get world map data
world_map <- map_data("world")

# Extract country names and latitudes
latitudes <- world_map %>%
  group_by(region) %>%
  summarize(LAT_country = mean(lat, na.rm = TRUE))%>%
  rename(region_world_map =region)

# Match country names to ISO3 codes
latitudes$adm0_a3 <- countrycode(latitudes$region_world_map, origin = "country.name", destination = "iso3c")

# Drop NA values (countries that couldn't be matched)
latitudes <- latitudes[!is.na(latitudes$adm0_a3), ]

latitudes<- latitudes %>%
  distinct(adm0_a3, .keep_all = TRUE)

# Merge with your dataset
final <- left_join(final, latitudes, by = "adm0_a3")

## ***********************************************
# Assign hemisphere
final <- final %>%
  mutate(Hemisphere = ifelse(is.na(Hemisphere), ifelse(LAT_country >= 0, "Northern", "Southern"), Hemisphere))

# Check if any missing hemispheres remain
sum(is.na(final$Hemisphere))


## ***********************************************
## research investment by country
## ***********************************************
year_columns <- grep("^X(200[0-9]|201[0-9]|2023)$", names(res.inv))

res.inv$PropGDP_median <- apply(res.inv[, year_columns], 1, function(x) {
  if (sum(is.na(x)) <= 19) {
    median(x, na.rm = TRUE)
  } else {
    NA
  }
})


#rename iso3c column of df to help with merging
names(res.inv)[names(res.inv) == "Country.Code"] <- "ISO3"

#merge gdp_median with complete df
final <- left_join(final, res.inv[,c("ISO3", "PropGDP_median")], by="ISO3")

## convert proportion to $$ by multiplying gdp and prop
final$ResInvestTotal <- final$PropGDP_median*final$GDP.MEDIAN


hist(log(final$ResInvestTotal),
     main="Research invenstment 20 year median", xlab="Investment in US dollars (log)",
     ylab="Number of countries")


## ***********************************************
## Area and bees' diversity by country
## ***********************************************
## VAT= vatican, FM= micronesia
area.richness <- area.richness[, c("NAME", "ISO3", "AREA", "CL_Species")]

#dim(area.richness)
#area.richness <- area.richness[!area.richness$ISO3 %in% not.real.countries,]
#dim(area.richness)

## drop really small islands that are territories and would have been
## coded as part of the colonial empire
area.richness$AREA <- as.numeric(area.richness$AREA)

#since this is by country not colonial power
names(area.richness)[names(area.richness) == "ISO3"] <- "adm0_a3"


## merging networks and bee richness by country
final <- left_join(final, area.richness, by = "adm0_a3")

## ***********************************************
## Years since published
## ***********************************************
final <- final %>%
  mutate(years_since_pub = as.numeric(format(Sys.Date(), "%Y")) - Publi_Year)


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
# drop the codes for regions, and country groupings
## also drop North Korea because they don't report well
#not.real.countries <- c("PRK", "")
#not.real.countries <- c("WLD", "AFE", "AFW", "ARB", "CEB", "CSS", "EAP",
#                        "EAR", "EAS", "ECA", "ECS", "EMU", "EUU",
 #                       "FCS", "HIC", "HPC", "IBD", "IBT", "IDA",
#                        "IDB",
#                        "IDX", "INX", "LAC", "LCN", "LDC", "LIC",
#                        "LMC", "LMY", "LTE", "MEA", "MIC", "MNA",
#                        "NAC", "OED", "OSS", "PRE", "PSE", "PSS",
#                        "PST",
#                        "SAS", "SSA", "SSF", "SST", "TEA", "TEC",
#                        "TLA", "TMM", "TSA", "TSS", "UMC", "PRK",
#                        "", "VAT")


#only korea is dropped
#dim(final)
#final <- final[!final$adm0_a3 %in% not.real.countries,]
#dim(final)



#unique(final[is.na(final$Continent),]$adm0_a3)



# Dominica (DMA) and Grenada (GRD) are island nations located in the Caribbean. 
# Although they're islands, the Caribbean is typically considered part of the 
# Americas, and in most global datasets—such as those from the United Nations or
# the World Bank—Caribbean countries are grouped under North America for 
# continental classification. On the other hand, Mauritius (MUS) and Seychelles
# (SYC) are island nations situated in the Indian Ocean, off the eastern coast of
# Africa. Despite their insular geography, they are considered part of Africa in 
# geopolitical and geographic classifications, and are members of African regional
# organizations such as the African Union.

#final <- final %>%
#  mutate(
#    Continent = case_when(
#      adm0_a3 == "DMA" ~ "North America",
#      adm0_a3 == "GRD" ~ "North America",
#      adm0_a3 == "MUS" ~ "Africa",
#      adm0_a3 == "SYC" ~ "Africa",
#      TRUE ~ Continent  # keep existing continent values
#    )
#  )
#unique(final[is.na(final$Continent),]$adm0_a3)


## ***********************************************
write.csv(final, file = "saved/webs_complete.csv")










#reported gdp the whole 20 year... 



