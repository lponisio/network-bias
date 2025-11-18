rm(list=ls())
source("~/lab_paths.R")
local.path
setwd(local.path)

setwd("network-bias")
source("src/initialize_packages.R")
source("src/initalize_figure.R")

# -------------------------------------------------------
# Summary stats for manuscript
# -------------------------------------------------------
# Countries with data by continent (subset and percentages by adm0_a3)
AfricaWebSum<-sum(webs_country[webs_country$Continent=="Africa",]$Total_webs_by_country)

Africa_summary <- webs_country %>%
  filter(Continent == "Africa", .keep_all =TRUE)%>%
  mutate(percent=Total_webs_by_country/AfricaWebSum)

unique(webs_complete[webs_complete$Continent=="Africa",]$Web_Code_base)

## Get the top 6 countries by number of networks
# Total number of webs globally
GlobalWebSum <- sum(webs_country$Total_webs_by_country, na.rm = TRUE)

top_countries <- webs_country %>%
  dplyr::select(adm0_a3, Total_webs_by_country, Country) %>%
  mutate(percentGlobalWeb = Total_webs_by_country / GlobalWebSum) %>%
  arrange(desc(percentGlobalWeb)) %>%
  slice_head(n = 6)

# Check percent covered by the top countries
sum(top_countries$percentGlobalWeb)


# Count top 3 first authors within each of those countries
top_authors_by_continent <- webs_complete %>%
  filter(adm0_a3 %in% top_countries$adm0_a3,
         Total_webs_by_country != 0) %>% #filter the zero country placeholder webs
  count(adm0_a3, FirstAuthor, Continent,  name = "n_networks") %>%
  group_by(adm0_a3, Continent) %>%
  arrange(desc(n_networks)) %>%
  slice_head(n = 3) %>%
  ungroup()
top_authors_by_continent

# Count top 5 first authors in Southern Hemisphere countries (Global South proxy)
top_authors_global_S <- webs_complete %>%
  filter(Hemisphere == "Southern",
         Total_webs_by_country != 0) %>%
  count(adm0_a3, Continent, FirstAuthor, name = "n_networks") %>%
  group_by(adm0_a3, Continent) %>%
  arrange(desc(n_networks)) %>%
  slice_head(n = 5) %>%
  ungroup()
top_authors_global_S

# ---------------------------------------------------------------
# Chi-square test: Are top 6 countries overrepresented in sampling?
# ---------------------------------------------------------------

# Identify top 6 by networks
top6 <- top_countries$adm0_a3

# Observed = number of networks in top 6 vs. all other countries
observed <- c(
  sum(webs_country$Total_webs_by_country[webs_country$adm0_a3 %in% top6]),
  sum(webs_country$Total_webs_by_country[!webs_country$adm0_a3 %in% top6])
)
#top six countries account for X% of all networks
observed[1]/sum(observed)

# Expected = share of global land area
total_area <- sum(webs_country$AREA)
expected <- c(
  sum(webs_country$AREA[webs_country$adm0_a3 %in% top6]) / total_area,
  sum(webs_country$AREA[!webs_country$adm0_a3 %in% top6]) / total_area
)
#percentage of land of top6 counrtries
expected[1]

# Convert expected proportions to expected counts
expected <- expected * sum(observed)

# Run chi-square test
chisq_test_result <- chisq.test(x = observed, p = expected / sum(expected))

chisq_test_result
