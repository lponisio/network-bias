rm(list=ls())
library(tidyverse)
library(MASS)

setwd("network-bias")

## Load data from CSV (semicolon-separated)
nets <- read.csv("data/raw/network_reuse.csv", sep = ";")

## Convert "N/A" values in Continent and From_Database to NA and drop missing
nets$Continent[nets$Continent == "N/A"] <- NA
nets <- nets[!is.na(nets$Continent),]

nets$From_Database[nets$From_Database == "N/A"] <- NA
nets <- nets[!is.na(nets$From_Database),]

## Count how many times each network (Web_Code) appears, grouped by key metadata
reused.web <- nets %>%
  group_by(Web_Code, Web_Year, Web_Decade, Data_Status,
           Country, From_Database,
           ISO3, Biome_WWF, Continent, Hemisphere) %>%
  summarise(ReusedCount = n())

## Drop duplicate rows based on Web_Code
reused.web <- reused.web[!duplicated(reused.web$Web_Code), ]

## If a network is marked "Original", set its reuse count to 0
original <- reused.web$Data_Status %in% "Original"
reused.web$ReusedCount[original] <- 0

## Create frequency table of all networks by Web_Code
n_occur <- data.frame(table(nets$Web_Code))

## Calculate network age as of 2025
reused.web$Age <- 2024 - reused.web$Web_Year

## Scatterplot of reuse count vs. network age
plot(reused.web$ReusedCount ~ reused.web$Age)

## Subtract 1 reuse to adjust for initial publication (if reused once,
## it's seen twice in the dataset)
reused.web$ReusedCount <- reused.web$ReusedCount - 1

## Histogram of adjusted reuse counts
hist(reused.web$ReusedCount)

## Reorder continent levels for consistent plotting and modeling
reused.web$Continent <- factor(reused.web$Continent,
                               levels = c("North America",
                                          "South America",
                                          "Africa",
                                          "Europe",
                                          "Oceania", "Asia"))

## Fit a GLM (negative binomial) to model reuse count as a function of
## age, continent, and data source
mod.age <- glm.nb(ReusedCount ~ scale(Age) * Continent + From_Database,
                  data = reused.web)

## Model summary and AIC
summary(mod.age)
AIC(mod.age)


web.by.continent <- aggregate(list(WebCount= reused.web$Web_Code),
                              list(Continent=reused.web$Continent),
                              function(x) length(unique(x)))

sa.webs.reused <-  reused.web[reused.web$Continent ==
                              "South America",]

sub.sa <- sa.webs.reused[, c("Web_Code", "ReusedCount")]

sub.sa <- sub.sa[order(sub.sa$ReusedCount, decreasing = TRUE),]

sub.sa[1:20,]

## investigate the diego vazquez effect

vaz.webs <- reused.web$Web_Code[grepl("vazquez", reused.web$Web_Code)]


nets.no.vaz <- nets[!nets$Web_Code %in% vaz.webs,]

no.vaz.reused.web <- nets.no.vaz %>%
    group_by(Web_Code, Web_Year, Web_Decade,
             Data_Status="Reused", Country, From_Database,
             ISO3, Biome_WWF, Continent, Hemisphere) %>%
    summarise(ReusedCount = n())

no.vaz.reused.web$ReusedCount  <- no.vaz.reused.web$ReusedCount - 1

hist(no.vaz.reused.web$ReusedCount)

no.vaz.reused.web$Continent <- factor(no.vaz.reused.web$Continent,
                               levels=c("North America",
                                        "South America",
                                        "Africa",
                                        "Europe",
                                        "Oceania", "Asia"  ))


## age of each original network
no.vaz.reused.web$Age  <-  2022- no.vaz.reused.web$Web_Year


mod.age.no.vaz <- glm.nb(ReusedCount ~ scale(Age)*Continent +
                             From_Database,
    data=no.vaz.reused.web)


summary(mod.age.no.vaz)


pframe <- with(reused.web,
               expand.grid(Age=seq(min(scale(Age)),max(scale(Age)),
                                   length=51),
                           Continent=unique(Continent),
                           From_Database=unique(From_Database)))

## add predicted values (on response scale) to prediction frame
pframe$ReusedCount <- predict(mod.age, newdata=pframe,
                             type="response")

ggplot(reused.web, aes(x=Age, y=ReusedCount, col =Continent)) +
   geom_point() +
   geom_smooth(method = "glm", se = FALSE,
       method.args = list(family = "poisson"), linetype = "dashed")+
   geom_line(data=pframe)
   ## use prediction data here
          ## (inherits aesthetics etc. from main ggplot call)



ggplot(reused.web, aes(x=Age, y=ReusedCount, col =Continent)) +
   geom_point() +
   geom_smooth(method = "glm.nb", se = FALSE)  +
   geom_line(data=pframe)
   ## use prediction data here
          ## (inherits aesthetics etc. from main ggplot call)
