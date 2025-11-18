rm(list=ls())
source("~/lab_paths.R")
local.path
setwd(local.path)

setwd("network-bias")
source("src/initialize_packages.R")
source("src/initalize_models.R")

# -------------------------------------------------------
# Summary stats for manuscript
# -------------------------------------------------------
#data set cleaned for model
webs_complete <- read.csv("data/webs_country.csv")

# Countries with data by continent (subset and percentages by adm0_a3)
Africa_summary <- webs_complete %>%
  filter(Continent == "Africa", .keep_all =TRUE)%>%
  mutate( sum(Total_webs_by_country))


  mutate(percent = 100 * n / sum(n)) %>%
  arrange(desc(percent)) 

unique(Africa$Web_Code_base)

##  Get the top 6 countries by number of networks
top_countries <- webs %>%
  count(adm0_a3, name = "n_networks") %>%
  mutate(percent = 100 * n_networks / sum(n_networks)) %>%
  arrange(desc(n_networks)) %>%
  slice_head(n = 6) 
top_countries


# Count top 3 first authors within each of those countries
top_authors_by_country <- webs %>%
  filter(adm0_a3 %in% top_countries$adm0_a3) %>%
  count(adm0_a3, FirstAuthor, Continent,  name = "n_networks") %>%
  group_by(adm0_a3, Continent) %>%
  arrange(desc(n_networks)) %>%
  slice_head(n = 3) %>%
  ungroup()
top_authors_by_country

# Count top 5 first authors in Southern Hemisphere countries (Global South proxy)
top_authors_global_S <- webs %>%
  filter(Hemisphere == "Southern") %>%
  count(adm0_a3, Continent, FirstAuthor, name = "n_networks") %>%
  group_by(adm0_a3, Continent) %>%
  arrange(desc(n_networks)) %>%
  slice_head(n = 5) %>%
  ungroup()
top_authors_global_S

##  Calculate percent area of top 6 countries (relative to global AREA sum)
percent_area_top6 <- webs %>%
  distinct(adm0_a3, .keep_all = TRUE) %>%
  summarise(
    total_area_all = sum(AREA, na.rm = TRUE),
    total_area_top6 = sum(AREA[adm0_a3 %in% top_countries$adm0_a3],
                          na.rm = TRUE)
  ) %>%
  mutate(percent_area = total_area_top6 / total_area_all)

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
