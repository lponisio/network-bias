rm(list=ls())

## ***********************************************
source("~/lab_paths.R")
setwd(local.path)
source("network-bias/src/initailize.R")


## ***********************************************
## Summarizes reuse of repeatedly published networks
## by determinging how many times the unique web code
## occurs
## ***********************************************

webs_reuse_count <- webs_reuse%>%
  group_by(Web_Code,Web_Year )%>%
  summarise(RefCode_string = str_c(Ref_Code, collapse = "; "),
            Publication_string = str_c(Source, collapse = "; "))

webs_reuse_summary <- webs_reuse_count %>%
  mutate(
    Web_Code_base = str_extract(Web_Code, ".*?\\d{4}"),
    is_reused_in_refcode = str_detect(RefCode_string, Web_Code_base),
    RefCode_string = if_else(
      !is_reused_in_refcode,
      str_c(RefCode_string, Web_Code_base, sep = "; "),
      RefCode_string
    ),
    pub_count = str_count(RefCode_string, ";") 
  )




#webs <- merge(webs_reuse_count, webs)
webs <- full_join(webs, webs_reuse_summary, by = "Web_Code")


## ***********************************************
## Cleaning the webs dataframe
## ***********************************************
#columns to keep 
col_keep <- c("Web_Code", "pub_count","Web_Code_base", "Web_Year",
              "RefCode_string","Publication_string","LAT", "LONG", "Region",
              "ISO3", "Hemisphere", "Country")

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
## Adding in Countries with Zero published networks
## ***********************************************
## this is real data, there are no webs from these countries, so
## create 0 count data and add them to the data
## NA we count as true zeros. We extract all entities from the GDP report
## published by the world bank group that do not have published networks
## in our data set.
## https://databank.worldbank.org/reports.aspx?source=2&type=metadata&series=NY.GDP.MKTP.CD#


#rename iso3c column of df to help with merging
names(gdp)[names(gdp) == "Country.Code"] <- "ISO3"
names(gdp)[names(gdp) == "Country.Name"] <- "Country"

# Select columns for the years 2000 to 2023 using the correct pattern
year_columns <- grep("^X(200[0-9]|201[0-9]|2023)$", names(gdp))

# Calculate the row medians for those columns
gdp$GDP.MEDIAN <- apply(gdp[, year_columns], 1, median, na.rm = FALSE)

## remove countries with NA gdp median
dim(gdp)
gdp <- gdp[!is.na(gdp$GDP.MEDIAN),]
dim(gdp)

countries.no.webs <- gdp[!gdp$ISO3 %in% countries.with.webs, c("ISO3","Country")]

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
## Some reported countries in our dataset actually are
## dependency on other countries (ex, greenland). Therefore
## These countries will have a distinct adm0_a3 representing
## themselves (ie greenland is GRL) while their ISO3 will be
## the country for which they are dependent on (ie Denmark DNK).
## This is because the research and gdp are only reported for the ISO3
## and it includes the adm0_a3. It's impossible to disentangle. We
## handle this later on by doing density (dividing by total land mass) 
## of country and dependencies. 
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

#our current network data uses the adm0_a3 but its labeled ISO3
names(final)[names(final) == "ISO3"] <- "adm0_a3"

#don't have contintent in the data set
names(countries)[names(countries) == "continent"] <- "Continent"

#adding the ISO3 for each dependency 
final <- left_join(final, countries, by = "adm0_a3")

dependcies <-final[(final$adm0_a3 !=final$ISO3) &!is.na(final$ISO3) ,]
head(dependcies)

#checking for countries which have reported gdp but no data in the nartural
#earth dataset. For now, we may assume that their ISO3 will match the adm0_a3,
#but later on these a majority will get dropped since there's no reported 
#research investment or species richness
no_match <-final[is.na(final$ISO3),]
head(no_match)

# Step 1: Convert blank or NA ISO3 using the Country column
final$ISO3[is.na(final$ISO3) | final$ISO3 == ""] <- countrycode(
  final$Country[is.na(final$ISO3) | final$ISO3 == ""],
  origin = 'country.name',
  destination = 'iso3c'
)

no_match <-final[is.na(final$ISO3),]
head(no_match)

# Step 2: Fill in remaining NAs with adm0_a3
final$ISO3[is.na(final$ISO3) | final$ISO3 == ""] <- final$adm0_a3[is.na(final$ISO3) | final$ISO3 == ""]

## ***********************************************
## Now that we have our complete dataset clean
## and containing zeros, we can add in the country 
## level variable: gdp median, research investment 
## (per GDP and total), bee species richness, and
## area of country
## ***********************************************

#adding GDP median
final <- left_join(final, gdp[, c("ISO3", "GDP.MEDIAN")], by = join_by(ISO3))


#Research and development expenditure (% of GDP), 
#https://databank.worldbank.org/reports.aspx?source=2&type=metadata&series=GB.XPD.RSDV.GD.ZS#
#reported by the worldbank

#grab columns from 2000 to 2023
year_columns <- grep("^X(200[0-9]|201[0-9]|2023)$", names(res.inv))

#only take the median of the countries research investment if they reported at
#least 5 years of data between 2000 and 2023
res.inv$PropGDP_median <- apply(res.inv[, year_columns], 1, function(x) {
  if (sum(is.na(x)) <= 19) {
    median(x, na.rm = TRUE)
  } else {
    NA
  }
})


#rename iso3c column of res.inv to help with merging
names(res.inv)[names(res.inv) == "Country.Code"] <- "ISO3"

#merge gdp_median with complete df
final <- left_join(final, res.inv[,c("ISO3", "PropGDP_median")], by="ISO3")


## convert proportion to $$ by multiplying gdp and prop
final$ResInvestTotal <- final$PropGDP_median*final$GDP.MEDIAN


hist(log(final$ResInvestTotal),
     main="Research invenstment 20 year median", xlab="Investment in US dollars (log)",
     ylab="Number of countries")


# We used the Orr et al. 2021 dataset to assign countries area (km2) and bee 
# species richness

#subset and standardize df
area.richness <- area.richness[, c("NAME", "ISO3", "AREA", "CL_Species")]
area.richness$AREA <- as.numeric(area.richness$AREA)

#since this is by country not dependency
names(area.richness)[names(area.richness) == "ISO3"] <- "adm0_a3"

## merging networks and bee richness by country
final <- left_join(final, area.richness, by = "adm0_a3")

#In our analysis of country level affects on network occurence, we will need to
#drop countries with no reported research investment (gdp or exependiture driven)
#bee species, and area
drop_countries <- final[is.na(final$ResInvestTotal)|is.na(final$CL_Species)|is.na(final$AREA),]
write.csv(drop_countries, file = "cleaning/offically_dropped/drop_countries.csv")


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

# Assign hemisphere
final <- final %>%
  mutate(Hemisphere = ifelse(is.na(Hemisphere), ifelse(LAT_country >= 0, "Northern", "Southern"), Hemisphere))

# Check if any missing hemispheres remain
sum(is.na(final$Hemisphere))


## ***********************************************
## Adding in metrics for ease of modeling and plotting
## ***********************************************
#years since publication of network
final <- final %>%
  mutate(years_since_pub = as.numeric(format(Sys.Date(), "%Y")) - Web_Year)

#density of bee species by country
final$CL_Species_Density <- final$CL_Species/ final$AREA

#sum area of dependencies and country
densities <- final %>%
   distinct(adm0_a3, .keep_all = TRUE) %>%
   group_by(ISO3) %>%
   summarize(AREA_by_ISO3 =sum(AREA))

final <- left_join(final, densities, by = "ISO3")

#take the density for each
final$ResInvs_Density <- final$ResInvestTotal/ final$AREA_by_ISO3

## ***********************************************
#some countries are missing a continent. 
## ***********************************************
Continent_check <- final %>%
  distinct(adm0_a3, .keep_all = TRUE) %>%
  filter(is.na(Continent),
         !is.na(AREA) , 
         !is.na(ResInvs_Density) ,
         !is.na(CL_Species_Density))

# Make sure the join key matches
names(continents)[names(continents) == "ISO.alpha3.Code"] <- "adm0_a3"

dim(final)
sum(is.na(final$adm0_a3))
# Join UN continent info to the main dataframe
final <- left_join(final, continents[, c("adm0_a3", "Region.Name")], by = "adm0_a3")
dim(final)
sum(is.na(final$adm0_a3))

# Fill in missing 'Continent' values using 'Region.Name' from the UN dataset
final <- final %>%
  mutate(Continent = if_else(is.na(Continent), Region.Name, Continent)) %>%
  dplyr::select(-Region.Name)  # remove helper column if not needed

Continent_check <- final %>%
  distinct(adm0_a3, .keep_all = TRUE) %>%
  filter(is.na(Continent),
         !is.na(AREA) , 
         !is.na(ResInvs_Density) ,
         !is.na(CL_Species_Density))

unique(final$Continent)

dim(final)
sum(is.na(final$adm0_a3))
sum(is.na(final$Continent))
final <- final %>%
  mutate(Continent = case_when(
    Continent == "Americas" & Hemisphere == "Northern" ~ "North America",
    Continent == "Americas" & Hemisphere == "Southern" ~ "South America",
    TRUE ~ Continent
  ))
dim(final)
sum(is.na(final$Continent))

## ***********************************************
write.csv(final, file = "saved/webs_complete.csv")







