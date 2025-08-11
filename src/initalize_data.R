# =========================================================
# Load Required Data Files
# - This section loads all raw datasets necessary for the analysis.
# =========================================================

#Define directory

#Load data
webs <- read.csv("data/raw/webs.csv", sep = ";")                     # plant–pollinator network data
webs_reuse <- read.csv("data/raw/network_reuse.csv", sep = ";")      # reuse records for networks
res.inv <- read.csv("data/raw/research_expenditure.csv")             # research expenditure by country
gdp <- read.csv("data/raw/gdp.csv")                                  # GDP data by country
area.richness <- read.csv("data/raw/bees_by_country.csv")            # bee species richness and area
continents <- read.csv("data/raw/UNSD_continents.csv", sep = ";")    # UNSD continent classification

