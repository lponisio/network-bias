rm(list=ls())
## ***********************************************
#workingdirectory
source("~/lab_paths.R")
local.path
setwd(local.path)

#packages
library(performance)
library(lme4)
library(dplyr)
library(MASS)
library(cowplot)
library(ggplot2)
## library(ggeffects)
library(viridis)
#data
webs_complete <- read.csv("network-bias-saved/saved/webs_complete.csv")

savefilepath <- c("network-bias-saved/manuscript/figures")

## ***********************************************
## Biome models
## ***********************************************
## from manuscript currently
# To test whether network collection is related to the biome area in the Northern 
# and Southern hemispheres, we included the number of networks by country as a 
# response variable and the biome area as an explanatory variable in a generalized 
# linear model (GLM).  We included an interaction between biome area and Northern 
# vs. Southern hemispehere to allow the accumulation of network with are to vary 
# by hemisphere. 
## ***********************************************
#we now have 
webs_bycounty <- webs_complete %>%
  filter(BiomeCode != "NA")%>%
  distinct(ISO3, .keep_all = TRUE) 

biome.net.m2 <- glm(Total_webs_by_country ~ log(AREA_biome_total) * Hemisphere,
                    data = webs_bycounty,
                    family = "poisson")
summary(biome.net.m2)

performance::check_model(biome.net.m2)

## ***********************************************
## country-level variables
## ***********************************************
#from manuscript
# Next, to test whether different country-level variables affected the number of 
# networks, we fit a GLM  with number of networks collected in each country as a 
# response variable and country area (km$^2$), research investment, and the number
# of bee species (the major pollinator group in most networks) as explanatory 
# variables. We included an interaction with each of these variables and continent 
# to allow their slopes to vary by continent.  
## ***********************************************
webs_country <- webs_complete %>%
  distinct(ISO3, .keep_all = TRUE) %>%
  filter(!is.na(CL_Species))

webs_country <- webs_country[webs_country$Continent != "Oceania",]

#Idk if we want this? can update in cleaning pipeline if so
webs_country <- webs_country %>%
  mutate(Continent = ifelse(Continent == "Americas" & Hemisphere == "Northern", 
                            "Northern America", 
                            ifelse(Continent == "Americas" & Hemisphere == "Southern", 
                                   "Southern America", 
                                   Continent)))

webs_country$Continent <- factor(webs_country$Continent,
                                     levels=c("Northern America",
                                              "Southern America",
                                              "Africa",
                                              "Europe",
                                              "Asia"))

# Add a small constant (1)
webs_country$log_CL_Species <- log(webs_country$CL_Species + 1)

library(glmmTMB)
library(car)

country.mod_continent <- glm(Total_webs_by_country ~ scale(log(AREA)) +
                                   scale(log_CL_Species) + Continent +
                                scale(PropGDP_median),
                                 data=webs_country,
                                 ## ziformula = ~1,
                                 family = "poisson")

summary(country.mod_continent)
vif(country.mod_continent)

performance::check_model(country.mod_continent)

# Residuals plot
residuals <- residuals(country.mod_continent)
plot(residuals)

# Predicted vs. residuals
plot(predict(country.mod_continent), residuals)

## ***********************************************
## network re-use
## ***********************************************
#from manuscript
# Lastly, to test whether the network re-use was related to the country from 
# which the original study collected the network or simply the number of years 
# since it has been published, we fit a GLMM with the number of times a network 
# was re-used as the response variable, and time since the original paper was 
# published and the country as explanatory variables. Similar to the second model, 
# we included continent as a random effect. 
## ***********************************************
webs_reuse <- webs_complete %>%
  distinct(Web_Code, .keep_all = TRUE) %>%
  mutate(years_since_pub = as.numeric(format(Sys.Date(), "%Y")) - Publi_Year)

#ARG is the intercept :)
webs_reuse_mod <- glm(Use_Frequency ~ ISO3 +years_since_pub,
                             data=webs_reuse,
                             ## ziformula = ~1,
                             family = "poisson")

summary(country.mod_continent)

performance::check_model(country.mod_continent)


# Residuals plot
residuals <- residuals(country.mod_continent)
plot(residuals)
# Predicted vs. residuals
plot(predict(country.mod_continent), residuals)


## ***********************************************
#gonna put this in a function/source file later but I am trying to tidy up my 
#latex table code to automatically populate results



models <- list("Biome Model" = biome.net.m2, 
               "Country Model" = country.mod_continent, 
               "Webs Reuse Model" = webs_reuse_mod)

# Extract and tidy the results
model_results <- lapply(models, broom.mixed::tidy)

# Define a function to standardize columns
standardize_columns <- function(df, all_cols) {
  df[setdiff(all_cols, names(df))] <- NA
  df <- df[all_cols]
  return(df)
}

# Get all unique column names across all data frames
all_cols <- unique(unlist(lapply(model_results, names)))

# Standardize columns for each model result
standardized_results <- lapply(model_results, function(df) standardize_columns(df, all_cols))

# Combine results
combined_results <- do.call(rbind, lapply(names(standardized_results), function(name) {
  results <- standardized_results[[name]]
  results$model <- name
  return(results)
}))

# Round all columns except p-values to 3 digits
combined_results <- combined_results %>%
  mutate(across(where(is.numeric) & !starts_with("p.value"), ~ round(., 2)))

library(dplyr)
colnames(combined_results) <- make.names(colnames(combined_results))

# Select columns of interest
results_table <- combined_results %>%
  dplyr::select(model, term, estimate, std.error, statistic, p.value) %>%
  mutate(p.value = format(p.value, scientific = TRUE, digits = 3))

library(dplyr)
library(glue)
library(readr)


# Function to format terms with p < 0.001 as bold
format_term <- function(term, p_value) {
  if (as.numeric(p_value) < 0.001) {
    return(glue("\\textbf{{{term}}}"))
  } else {
    return(term)
  }
}
library(glue)
library(dplyr)

latex_table <- glue(
  "\\begin{{sidewaystable}}
  \\centering
  \\footnotesize
  \\caption{{Parameter estimates and standard errors for the biome and country models. Terms with strong support ($p < 0.001$) are bolded.}}
  \\label{{tab:results-models}}
  \\resizebox{{0.9\\textwidth}}{{!}}{{
  \\begin{{tabular}}{{|l|l|c|c|c|c|}}
  \\hline
  \\textbf{{Model}} & \\textbf{{Term}} & \\textbf{{Estimate}} & \\textbf{{Std. Error}} & \\textbf{{Statistic}} & \\textbf{{p-value}} \\\\
  \\hline
  {results_table %>%
    mutate(term = mapply(format_term, term, p.value)) %>%
    group_by(model) %>%
    summarise(latex_rows = paste0(
      model, " & ", term, 
      " & ", estimate, 
      " & ", std.error, 
      " & ", statistic, 
      " & ", p.value, "\\\\"
    )) %>%
    pull(latex_rows) %>%
    paste(collapse = "\n")
  }
  \\hline
  \\end{{tabular}}
  }}
  \\end{{sidewaystable}}"
)

cat(latex_table)  # Print to verify output


# Save to file
write_lines(latex_table, "results_table.tex")

# Print LaTeX table
cat(latex_table)


