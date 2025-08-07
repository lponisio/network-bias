rm(list=ls())

setwd("network-bias")
source("src/initalize.R")

## ******************************************************************
## Summarizes reuse of repeatedly published networks
## by determinging how many times the unique web code
## occurs
## ******************************************************************

## Remove suffixes from Ref_Code (e.g., "_1", "_2") to collapse repeat
## publications
webs_reuse_count <- webs_reuse %>%
  mutate(
    Ref_Code_fixed = str_replace(Ref_Code, "(?<=\\d{4})_\\d+$", "")
  ) %>%
  group_by(Web_Code, Web_Year) %>%
  summarise(
    RefCode_string = str_c(Ref_Code_fixed, collapse = "; "),  # Concatenate all source Ref_Codes
    Publication_string = str_c(Source, collapse = "; "),      # Concatenate publication sources
    .groups = "drop"
  )

## Create a summary to flag reused networks
webs_reuse_summary <- webs_reuse_count %>%
  mutate(
    Web_Code_base = Web_Code %>%
      str_replace("(?<=\\d{4})(_[A-Za-z]{2,}\\d*|_\\d+)$", ""), # Base code without suffix
    is_reused_in_refcode = str_detect(RefCode_string, Web_Code_base),
    RefCode_string = if_else(
      !is_reused_in_refcode,
      str_c(RefCode_string, Web_Code_base, sep = "; "),
      RefCode_string
    ),
    pub_count = str_count(RefCode_string, ";")  # Number of publications per web
  )

## Keep only one row per unique Web_Code in the original data
webs_reuse <- webs_reuse %>%
  distinct(Web_Code, .keep_all = TRUE)

## Merge summary with original data
webs_reuse_summary <- full_join(webs_reuse, webs_reuse_summary,
                                by = c("Web_Code", "Web_Year"))

## Keep only relevant metadata for reuse summary
col_keep <- c("Web_Code", "pub_count","Web_Code_base", "Web_Year",
              "RefCode_string","Publication_string", 
              "ISO3", "Hemisphere", "Country")

webs_reuse_summary <- webs_reuse_summary[, col_keep, drop = FALSE]

# Merge with main webs data
webs <- full_join(webs, webs_reuse_summary,
                  by = c("Web_Code", "Country","ISO3","Hemisphere"))

## ******************************************************************
## Cleaning the webs dataframe
## ******************************************************************

## Drop unsed columns, keep geographic and metadata
col_keep <- c("Web_Code", "pub_count","Web_Code_base", "Web_Year",
              "RefCode_string","Publication_string","LAT", "LONG", "Region",
              "ISO3", "Hemisphere", "Country")
webs <- webs[, col_keep, drop = FALSE]

## Standardize Country and ISO3 codes using countrycode package
countries <- unique(webs$Country)
webs$Country <- countrycode(as.character(webs$Country),
                            origin = "country.name", destination = "country.name")
new_countries <- unique(webs$Country)
paste(setdiff(countries,new_countries), "to", setdiff(new_countries,countries))

ISO3 <- unique(webs$ISO3)
webs$ISO3 <- countrycode(as.character(webs$Country),
                         origin = "country.name", destination = "iso3c")

new_ISO3 <- unique(webs$ISO3)
paste(setdiff(ISO3, new_ISO3), "to", setdiff(new_ISO3, ISO3))
## no changes

## Webs without countries
dim(webs)
webs_na <- webs %>% 
            filter(is.na(Country) & Country != "",
                  is.na(ISO3) & ISO3 != "")

webs_na
## No webs without countries

## Represented countries
countries.with.webs <- sort(unique(webs$Country))
print("Countries with webs")
countries.with.webs

# Count number of networks by country (ISO3)
web_country <- webs %>%
  group_by(ISO3)%>%
  summarise(Total_webs_by_country = n())

webs <- left_join(web_country, webs, by = "ISO3")

## ******************************************************************
## Adding in Countries with Zero published networks
## ******************************************************************

## This is real data, there are no webs from these countries, so
## create 0 count data and add them to the data
## NA we count as true zeros. We extract all entities from the GDP report
## published by the world bank group that do not have published networks
## in our data set.
## https://databank.worldbank.org/reports.aspx?source=2&type=metadata&series=NY.GDP.MKTP.CD#


## Columns in GDP that are not countries:
not.countries <- c("Africa Eastern and Southern",
                   "Africa Western and Central", "Arab World",
                   "Central Europe and the Baltics",
                   "Channel Islands", "Caribbean small states",
                   "East Asia & Pacific (excluding high income)",
                   "Early-demographic dividend",
                   "East Asia & Pacific",
                   "Europe & Central Asia (excluding high income)",
                   "Europe & Central Asia", "Euro area",
                   "European Union",
                   "Fragile and conflict affected situations",
                   "High income",
                   "Heavily indebted poor countries (HIPC)",
                   "IBRD only", "IDA & IBRD total", "IDA total",
                   "IDA blend", "IDA only", "Not classified",
                   "Latin America & Caribbean (excluding high income)",
                   "Latin America & Caribbean",
                   "Least developed countries: UN classification",
                   "Low income", "Lower middle income",
                   "Low & middle income", "Late-demographic dividend",
                   "Middle East & North Africa", "Middle income",
                   "Middle East & North Africa (excluding high income)",
                   "North America", "OECD members",
                   "Other small states", "Pre-demographic dividend",
                   "Pacific island small states",
                   "Post-demographic dividend", "South Asia",
                   "Sub-Saharan Africa (excluding high income)",
                   "Sub-Saharan Africa", "Small states",
                   "East Asia & Pacific (IDA & IBRD countries)",
                   "Europe & Central Asia (IDA & IBRD countries)",
                   "Latin America & the Caribbean (IDA & IBRD countries)",
                   "Middle East & North Africa (IDA & IBRD countries)",
                   "South Asia (IDA & IBRD)",
                   "Sub-Saharan Africa (IDA & IBRD countries)",
                   "Upper middle income", "World")

gdp <- gdp[!gdp$Country %in% not.countries,]

## Rename iso3c column of df to help with merging
names(gdp)[names(gdp) == "Country.Code"] <- "ISO3"
names(gdp)[names(gdp) == "Country.Name"] <- "Country"

## Select columns for the years 2000 to 2023 using the correct pattern
year_columns <- grep("^X(200[0-9]|201[0-9]|2023)$", names(gdp))

## Calculate the row medians for those columns
gdp$GDP.MEDIAN <- apply(gdp[, year_columns], 1, median, na.rm = FALSE)

countries.no.webs <- gdp[!gdp$ISO3 %in% countries.with.webs, c("ISO3","Country")]
countries.no.webs$Total_webs_by_country <- 0

## Add missing columns to no.webs (fill with NA or a default value)
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
## Some reported countries in our dataset actually are dependency on
## other countries (ex, greenland). Therefore These countries will
## have a distinct adm0_a3 representing themselves (ie greenland is
## GRL) while their ISO3 will be the country for which they are
## dependent on (ie Denmark DNK).  This is because the research and
## gdp are only reported for the ISO3 and it includes the
## adm0_a3. It's impossible to disentangle. We handle this later on by
## doing density (dividing by total land mass) of country and
## dependencies.

## ***********************************************
## Handle colonies or sovereignt states! Add a new column, one
## for the sovereignt (iso3c)or the colonial country and one for the
## colony (admin or adm0_a3)

# Match ISO3 for sovereign states and dependencies using rnaturalearth
countries <- rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_drop_geometry() %>%
  dplyr::select(sovereignt, admin,  adm0_a3, continent, type)%>%
  mutate(
    ISO3 = countrycode(sovereignt, origin = 'country.name', destination = 'iso3c'))

## Final data uses adm0_a3 as ISO3 label for now
names(final)[names(final) == "ISO3"] <- "adm0_a3"
names(countries)[names(countries) == "continent"] <- "Continent"

## Merge sovereign info into final dataset
final <- left_join(final, countries, by = "adm0_a3")

## Fill missing ISO3 using Country name or fallback to adm0_a3
final$ISO3[is.na(final$ISO3) | final$ISO3 == ""] <-
  countrycode(final$Country[is.na(final$ISO3) | final$ISO3 == ""],
              origin = 'country.name', destination = 'iso3c')
final$ISO3[is.na(final$ISO3) | final$ISO3 == ""] <-
  final$adm0_a3[is.na(final$ISO3) | final$ISO3 == ""]


dependcies <-final[(final$adm0_a3 !=final$ISO3) &!is.na(final$ISO3) ,]
sort(unique(dependcies$Country))

## Checking for countries which have reported gdp but no data in the
## nartural earth dataset. For now, we may assume that their ISO3 will
## match the adm0_a3, but later on these a majority will get dropped
## since there's no reported research investment or species richness
no_match <-final[is.na(final$ISO3),]
head(no_match)

## Step 1: Convert blank or NA ISO3 using the Country column
final$ISO3[is.na(final$ISO3) | final$ISO3 == ""] <- countrycode(
  final$Country[is.na(final$ISO3) | final$ISO3 == ""],
  origin = 'country.name',
  destination = 'iso3c'
)

no_match <-final[is.na(final$ISO3),]
head(no_match)

## Step 2: Fill in remaining NAs with adm0_a3
final$ISO3[is.na(final$ISO3) | final$ISO3 == ""] <-
  final$adm0_a3[is.na(final$ISO3) | final$ISO3 == ""]

## ******************************************************************
## Now that we have our complete dataset clean and containing zeros,
## we can add in the country level variable: gdp median, research
## investment (per GDP and total), bee species richness, and area of
## country
## ******************************************************************

# Merge GDP median into final dataset
final <- left_join(final, gdp[, c("ISO3", "GDP.MEDIAN")], by = join_by(ISO3))

## Research and development expenditure (% of GDP),
## https://databank.worldbank.org/reports.aspx?source=2&type=metadata&series=GB.XPD.RSDV.GD.ZS##
## reported by the worldbank

# Calculate research investment median from 2000–2023 if at least 5 years reported
year_columns <- grep("^X(200[0-9]|201[0-9]|2023)$", names(res.inv))
res.inv$PropGDP_median <- apply(res.inv[, year_columns], 1, function(x) {
  if (sum(is.na(x)) <= 19) median(x, na.rm = TRUE) else NA
})

# Merge research data
names(res.inv)[names(res.inv) == "Country.Code"] <- "ISO3"
final <- left_join(final, res.inv[,c("ISO3", "PropGDP_median")], by="ISO3")

# Convert proportion to absolute dollars
final$ResInvestTotal <- final$PropGDP_median * final$GDP.MEDIAN

hist(log(final$ResInvestTotal),
     main="Research invenstment 20 year median", xlab="Investment in US dollars (log)",
     ylab="Number of countries")

## ******************************************************************
## We used the Orr et al. 2021
## (https://doi.org/10.1016/j.cub.2020.10.053) dataset to assign
## countries area (km2) and bee species richness

# Standardize and merge in area and bee species richness
area.richness <- area.richness[, c("NAME", "ISO3", "AREA", "CL_Species")]
area.richness$AREA <- as.numeric(area.richness$AREA)
names(area.richness)[names(area.richness) == "ISO3"] <- "adm0_a3"
final <- left_join(final, area.richness, by = "adm0_a3")

# Save dropped countries with missing key data
drop_countries <- final[is.na(final$ResInvestTotal)|
                        is.na(final$CL_Species)|is.na(final$AREA),]
write.csv(drop_countries, file = "cleaning/offically_dropped/drop_countries.csv")

## ******************************************************************

# Use world map data to infer country latitude → hemisphere
world_map <- map_data("world")
latitudes <- world_map %>%
  group_by(region) %>%
  summarize(LAT_country = mean(lat, na.rm = TRUE)) %>%
  rename(region_world_map = region)

latitudes$adm0_a3 <- countrycode(latitudes$region_world_map,
                                 origin = "country.name", destination = "iso3c")
latitudes <- latitudes[!is.na(latitudes$adm0_a3), ] %>%
  distinct(adm0_a3, .keep_all = TRUE)

# Merge into final and assign hemisphere
final <- left_join(final, latitudes, by = "adm0_a3")
final <- final %>%
  mutate(Hemisphere = ifelse(is.na(Hemisphere),
                      ifelse(LAT_country >= 0,
                             "Northern", "Southern"),
                      Hemisphere))

## Check if any missing hemispheres remain
sum(is.na(final$Hemisphere))

## ******************************************************************
## Derived metrics
## ******************************************************************

## Time since publication
final <- final %>%
  mutate(years_since_pub = as.numeric(format(Sys.Date(), "%Y")) - Web_Year)

## Bee richness per area
final$CL_Species_Density <- final$CL_Species / final$AREA

## Sum total area of sovereign country and its dependencies
densities <- final %>%
  distinct(adm0_a3, .keep_all = TRUE) %>%
  group_by(ISO3) %>%
  summarize(AREA_by_ISO3 = sum(AREA))

final <- left_join(final, densities, by = "ISO3")

## Research investment per km²
final$ResInvs_Density <- final$ResInvestTotal / final$AREA_by_ISO3


## ******************************************************************
## Update missing continents
## ******************************************************************

continent_check <- final %>%
  distinct(adm0_a3, .keep_all = TRUE) %>%
  filter(is.na(Continent),
         !is.na(AREA) , 
         !is.na(ResInvs_Density) ,
         !is.na(CL_Species_Density))
continent_check

names(continents)[names(continents) == "ISO.alpha3.Code"] <- "adm0_a3"
final <- left_join(final, continents[, c("adm0_a3", "Region.Name")], by = "adm0_a3")

# Use UN region if continent still NA
final <- final %>%
  mutate(Continent = if_else(is.na(Continent), Region.Name, Continent)) %>%
  dplyr::select(-Region.Name)

# Split Americas into North/South using hemisphere
final <- final %>%
  mutate(Continent = case_when(
    Continent == "Americas" & Hemisphere == "Northern" ~ "North America",
    Continent == "Americas" & Hemisphere == "Southern" ~ "South America",
    TRUE ~ Continent
  ))

continent_check <- final %>%
  distinct(adm0_a3, .keep_all = TRUE) %>%
  filter(is.na(Continent),
         !is.na(AREA) , 
         !is.na(ResInvs_Density) ,
         !is.na(CL_Species_Density))
continent_check

unique(final$Continent)

sum(is.na(final$adm0_a3))
sum(is.na(final$Continent))

final$Country[is.na(final$Continent)]


## Export final dataset
write.csv(final, file = "saved/webs_complete.csv")
