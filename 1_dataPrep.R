rm(list=ls())

setwd("network-bias")
source("src/initalize_packages.R")
source("src/initalize_data.R")

# =========================================================
# Summarize Reuse of Repeatedly Published Networks
#
# Identifying plant–pollinator networks that have been published more 
# than once, consolidate their references, and count how many times 
# each unique network "Web_Code" appears in the literature.
# =========================================================

# Remove suffixes from Ref_Code to collapse repeated publications
webs_reuse_count <- webs_reuse %>%
  mutate(
    Ref_Code_fixed = str_replace(Ref_Code, "(?<=\\d{4})_\\d+$", "")  # remove suffix after year
  ) %>%
  group_by(Web_Code, Web_Year) %>%
  summarise(
    RefCode_string = str_c(Ref_Code_fixed, collapse = "; "),  # concatenate all cleaned Ref_Codes
    Publication_string = str_c(Source, collapse = "; "),      # concatenate publication sources
    .groups = "drop"
  )

# Create a summary to flag reused networks and count publications
webs_reuse_summary <- webs_reuse_count %>%
  mutate(
    Web_Code_base = Web_Code %>%
      str_replace("(?<=\\d{4})(_[A-Za-z]{2,}\\d*|_\\d+)$", ""), # base code without suffix
    is_reused_in_refcode = str_detect(RefCode_string, Web_Code_base), # flag reused networks
    RefCode_string = if_else(
      !is_reused_in_refcode,
      str_c(RefCode_string, Web_Code_base, sep = "; "),  # add base code if missing
      RefCode_string
    ),
    pub_count = str_count(RefCode_string, ";")  # count number of publications per network
  )

# Keep only one row per unique Web_Code in the original data
webs_reuse <- webs_reuse %>%
  distinct(Web_Code, .keep_all = TRUE)

# Merge reuse summary with original metadata
webs_reuse_summary <- full_join(
  webs_reuse,
  webs_reuse_summary,
  by = c("Web_Code", "Web_Year"))

# Select only relevant columns for reuse summary
col_keep <- c("Web_Code", "pub_count", "Web_Code_base", "Web_Year",
              "RefCode_string", "Publication_string", 
              "ISO3", "Hemisphere", "Country")

webs_reuse_summary <- webs_reuse_summary[, col_keep, drop = FALSE]

# Merge with main webs data
webs <- full_join(webs, webs_reuse_summary,
                  by = c("Web_Code", "Country","ISO3","Hemisphere"))


# =========================================================
# Clean the 'webs' Dataframe
#
# Preparing the plant–pollinator network metadata for analysis
# =========================================================

# Keep only relevant columns: geographic coordinates, country info, and metadata
col_keep <- c("Web_Code", "pub_count", "Web_Code_base", "Web_Year",
              "RefCode_string", "Publication_string", "LAT", "LONG", "Region",
              "ISO3", "Hemisphere", "Country")
webs <- webs[, col_keep, drop = FALSE]

# Standardize country names using the 'countrycode' package
countries <- unique(webs$Country)
webs$Country <- countrycode(as.character(webs$Country),
                            origin = "country.name", destination = "country.name")

# Check if any country names were changed during standardization
new_countries <- unique(webs$Country)
paste(setdiff(countries, new_countries), "to", setdiff(new_countries, countries))

# Standardize ISO3 codes based on country names
ISO3 <- unique(webs$ISO3)
webs$ISO3 <- countrycode(as.character(webs$Country),
                         origin = "country.name", destination = "iso3c")

# Check if any ISO3 codes were changed during standardization
new_ISO3 <- unique(webs$ISO3)
paste(setdiff(ISO3, new_ISO3), "to", setdiff(new_ISO3, ISO3))
# No changes detected

# Identify webs without country or ISO3 information
dim(webs)
webs_na <- webs %>% 
  filter(is.na(Country) & Country != "",
         is.na(ISO3) & ISO3 != "")

webs_na
# No networks without countries

# List represented countries in alphabetical order
countries.with.webs <- sort(unique(webs$Country))
print("Countries with webs")
countries.with.webs

# Count number of networks per country (by ISO3 code)
web_country <- webs %>%
  group_by(ISO3) %>%
  summarise(Total_webs_by_country = n())

# Merge network counts back into the main dataframe
webs <- left_join(web_country, webs, by = "ISO3")

# =========================================================
# Clean GDP Data and Add Countries with Zero Published Networks
#
# Including countries that have no recorded plant–pollinator networks 
# in the dataset, assigning them a network count of zero. 
#
# Details:
# - Clean the World Bank GDP dataset by removing non-country entities 
#   (e.g., regional groupings, income categories, aggregates).
# - Standardize country names and ISO3 codes for consistency.
# - Select GDP data for the period 2000–2023 and calculate the median 
#   GDP per country.
# - Identify countries with no recorded plant–pollinator networks and 
#   assign them a network count of zero.
# - Merge these zero-network countries into the main dataset so all 
#   recognized countries are represented.
# 
# Source for GDP metadata:
# https://databank.worldbank.org/reports.aspx?source=2&type=metadata&series=NY.GDP.MKTP.CD#
# =========================================================

# Remove non-country entries from the GDP dataset
not.countries <- c(
  "Africa Eastern and Southern", "Africa Western and Central", "Arab World",
  "Central Europe and the Baltics", "Channel Islands", "Caribbean small states",
  "East Asia & Pacific (excluding high income)", "Early-demographic dividend",
  "East Asia & Pacific", "Europe & Central Asia (excluding high income)",
  "Europe & Central Asia", "Euro area", "European Union",
  "Fragile and conflict affected situations", "High income",
  "Heavily indebted poor countries (HIPC)", "IBRD only", "IDA & IBRD total",
  "IDA total", "IDA blend", "IDA only", "Not classified",
  "Latin America & Caribbean (excluding high income)", "Latin America & Caribbean",
  "Least developed countries: UN classification", "Low income", "Lower middle income",
  "Low & middle income", "Late-demographic dividend", "Middle East & North Africa",
  "Middle income", "Middle East & North Africa (excluding high income)",
  "North America", "OECD members", "Other small states", "Pre-demographic dividend",
  "Pacific island small states", "Post-demographic dividend", "South Asia",
  "Sub-Saharan Africa (excluding high income)", "Sub-Saharan Africa", "Small states",
  "East Asia & Pacific (IDA & IBRD countries)",
  "Europe & Central Asia (IDA & IBRD countries)",
  "Latin America & the Caribbean (IDA & IBRD countries)",
  "Middle East & North Africa (IDA & IBRD countries)",
  "South Asia (IDA & IBRD)", "Sub-Saharan Africa (IDA & IBRD countries)",
  "Upper middle income", "World")

# Keep only actual countries; remove regional/income group aggregates
gdp <- gdp[!gdp$Country.Name %in% not.countries, ]

# Standardize column names for merging
names(gdp)[names(gdp) == "Country.Code"] <- "ISO3"
names(gdp)[names(gdp) == "Country.Name"] <- "Country"

# Select columns for years 2000–2023
year_columns <- grep("^X(200[0-9]|201[0-9]|2023)$", names(gdp))

# Calculate median GDP over the selected years
gdp$GDP.MEDIAN <- apply(gdp[, year_columns], 1, median, na.rm = FALSE)

# Identify countries with zero published networks
countries.no.webs <- gdp[!gdp$ISO3 %in% countries.with.webs, c("ISO3", "Country")]
countries.no.webs$Total_webs_by_country <- 0

# Add any missing columns (fill with NA)
missing_cols <- setdiff(colnames(webs), colnames(countries.no.webs))
for (col in missing_cols) {
  countries.no.webs[[col]] <- NA
}

# Ensure column order matches main dataset
countries.no.webs <- countries.no.webs[, colnames(webs)]

# Combine datasets (existing + zero-network countries)
final <- rbind(webs, countries.no.webs)

# =========================================================
# Observation:
# Some countries reported in the dataset are actually dependencies 
# of other countries (e.g., Greenland). These dependencies have their 
# own administrative code (adm0_a3), such as "GRL" for Greenland, 
# but share the ISO3 code of the country they depend on (e.g., "DNK" 
# for Denmark). Research investment and GDP data are reported only for 
# the ISO3 of the sovereign country, which includes its dependencies.
#   
# - This makes it impossible to fully separate the dependency’s data 
#   from its sovereign state in national-level statistics.
#
# Implication:
# We address this later by calculating densities (e.g., per unit of 
# land area) that combine the country and its dependencies.
# =========================================================

# =========================================================
# Handling with Sovereign States and Dependencies
#
# - Attach sovereign-state metadata (sovereignt, admin, adm0_a3, type, continent)
#   from Natural Earth to our combined dataset.
# - Maintain two identifiers:
#   * adm0_a3  : administrative (country/dependency) 3-letter code
#   * ISO3     : sovereign ISO3 code used for national statistics (GDP, research)
# =========================================================

# Lookup table from Natural Earth (drop geometry and compute ISO3 for sovereigns)
countries <- rnaturalearth::ne_countries(returnclass = "sf") %>%
  sf::st_drop_geometry() %>%
  dplyr::select(sovereignt, admin, adm0_a3, continent, type) %>%
  dplyr::mutate(
    ISO3 = countrycode(sovereignt, origin = "country.name", destination = "iso3c")
  )

# Align column names to join keys / naming used downstream
names(final)[names(final) == "ISO3"] <- "adm0_a3"     # keep adm0_a3 as the current join key
names(countries)[names(countries) == "continent"] <- "Continent"

# Merge sovereign/dependency metadata into the main dataset by adm0_a3
final <- dplyr::left_join(final, countries, by = "adm0_a3")

# Backfill missing ISO3:
# 1) try deriving from Country name
# 2) if still missing, fallback to adm0_a3 (treat admin code as ISO3)
missing_iso3 <- is.na(final$ISO3) | final$ISO3 == ""
final$ISO3[missing_iso3] <- countrycode(final$Country[missing_iso3],
                                        origin = "country.name", destination = "iso3c")
missing_iso3 <- is.na(final$ISO3) | final$ISO3 == ""
final$ISO3[missing_iso3] <- final$adm0_a3[missing_iso3]

# Flag and list dependencies: entries where administrative code != sovereign ISO3
dependencies <- final[(final$adm0_a3 != final$ISO3) & !is.na(final$ISO3), ]
sort(unique(dependencies$Country))

# =========================================================
# Reconcile ISO3 Codes Missing from Natural Earth
#
# Identify records that have GDP (country present in World Bank data)
# but lack a matching ISO3 from Natural Earth. Attempt to recover ISO3
# using the Country name; if still missing, fall back to adm0_a3 so
# these rows remain traceable. Many of these may be dropped later if
# they lack research investment or bee species richness data.
# =========================================================

# Inspect rows with missing ISO3 (no Natural Earth match)
no_match <- final[is.na(final$ISO3), ]
head(no_match)

# Derive ISO3 from Country name via countrycode
final$ISO3[is.na(final$ISO3) | final$ISO3 == ""] <- countrycode(
  final$Country[is.na(final$ISO3) | final$ISO3 == ""],
  origin = "country.name",
  destination = "iso3c")

# Check remaining unmatched cases
no_match <- final[is.na(final$ISO3), ]
head(no_match)

# Fallback: use adm0_a3 where ISO3 is still missing/blank
final$ISO3[is.na(final$ISO3) | final$ISO3 == ""] <- 
  final$adm0_a3[is.na(final$ISO3) | final$ISO3 == ""]

# =========================================================
# Add Country-level Predictors
# Research & Development Expenditure (World Bank):
# https://databank.worldbank.org/reports.aspx?source=2&type=metadata&series=GB.XPD.RSDV.GD.ZS##
#
# - Derive a country-level R&D investment metric from World Bank data:
#   * PropGDP_median: median R&D expenditure (% of GDP) across 2000–2023
#   * ResInvestTotal: absolute R&D spending (USD), derived from GDP.MEDIAN
#
# Enrich the unified dataset (`final`) with national covariates:
# - GDP.MEDIAN (median GDP 2000–2023, World Bank)
# - Research investment (total and per-GDP)          
# - Bee species richness                              
# - Country land area (km^2)                          
# =========================================================

# Attach median GDP by ISO3; keep all rows in `final` data
final <- dplyr::left_join(
  final,
  gdp[, c("ISO3", "GDP.MEDIAN")],
  by = dplyr::join_by(ISO3))  

# Select year columns 
year_columns <- grep("^X(200[0-9]|201[0-9]|2023)$", names(res.inv))

# Median R&D as % of GDP over available years (require at least 5 reported years)
res.inv$PropGDP_median <- apply(res.inv[, year_columns], 1, function(x) {
  if (sum(!is.na(x)) >= 5) median(x, na.rm = TRUE) else NA_real_
})

# Prepare for merge
names(res.inv)[names(res.inv) == "Country.Code"] <- "ISO3"

# Merge median R&D (% of GDP) into `final`
final <- dplyr::left_join(final, res.inv[, c("ISO3", "PropGDP_median")], by = "ISO3")

# Convert from % of GDP to absolute USD
# If PropGDP_median is in percent (typical WB series), divide by 100; if already in proportion, keep as is.
scale_factor <- ifelse(stats::median(final$PropGDP_median, na.rm = TRUE) > 1, 0.01, 1)
final$ResInvestTotal <- final$PropGDP_median * scale_factor * final$GDP.MEDIAN  # USD

# Quick diagnostic plot (log of positive values only to avoid -Inf)
ri_pos <- final$ResInvestTotal[is.finite(final$ResInvestTotal) & final$ResInvestTotal > 0]
hist(log(ri_pos),
     main = "Research investment (median 2000–2023)",
     xlab = "Investment in US dollars (log)",
     ylab = "Number of countries")

# =========================================================
# Country Area (km^2) & Bee Species Richness — Orr et al. (2021)
#
# - Orr et al. 2021 dataset with columns: NAME, ISO3, AREA, CL_Species.
# - Source: https://doi.org/10.1016/j.cub.2020.10.053
# =========================================================

# Standardize columns and types, then merge
area.richness <- area.richness[, c("NAME", "ISO3", "AREA", "CL_Species")]
area.richness$AREA <- as.numeric(area.richness$AREA)
names(area.richness)[names(area.richness) == "ISO3"] <- "adm0_a3"

final <- dplyr::left_join(final, area.richness, by = "adm0_a3")

# Save countries with missing key data (ensure folder exists)
drop_countries <- final[is.na(final$ResInvestTotal) |
                          is.na(final$CL_Species)    |
                          is.na(final$AREA), ]

write.csv(drop_countries,
          file = "cleaning/offically_dropped/drop_countries.csv",
          row.names = FALSE)


# =========================================================
# Infer Hemisphere from Country Mean Latitude (World Map)
# =========================================================

# World map mean latitude by region
# Use world map data to infer country latitude to hemisphere
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

# =========================================================
# Derived Metrics
#
# - Create analysis-ready variables:
#   • years_since_pub: years since original publication (uses system year)
#   • CL_Species_Density: bee richness per km²
#   • AREA_by_ISO3: total land area of sovereign country + dependencies
#   • ResInvs_Density: research investment per km²
# =========================================================

## Time since publication
final <- final %>%
  mutate(years_since_pub = as.numeric(format(Sys.Date(), "%Y")) - Web_Year)  # current year - Web_Year

## Bee richness per area
final$CL_Species_Density <- final$CL_Species / final$AREA  # species per km²

## Sum total area of sovereign country and its dependencies
densities <- final %>%
  distinct(adm0_a3, .keep_all = TRUE) %>%
  group_by(ISO3) %>%
  summarize(AREA_by_ISO3 = sum(AREA))  # km² summed within sovereign ISO3

final <- left_join(final, densities, by = "ISO3")  # attach AREA_by_ISO3

## Research investment per km²
final$ResInvs_Density <- final$ResInvestTotal / final$AREA_by_ISO3  # USD per km²

# =========================================================
# Update Missing Continents
#
# - Backfill missing `Continent` in `final` using UNSD region mapping,
#   then split "Americas" into "North America" and "South America"
#   based on `Hemisphere`. Includes sanity checks and export.
# =========================================================

# Update missing continents
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

webs_complete <- final

# Export final dataset
write.csv(final, file = "data/webs_complete.csv")


