rm(list=ls())
library(tidyverse)
library(dplyr)

setwd("network-bias")

#reading the data
nets <- read.csv("network_reuse.csv", sep=";")

save(nets, file="network_reuse.Rdata")

load(file="network_reuse.Rdata")


nets$Continent[nets$Continent == "N/A"] <- NA
nets <- nets[!is.na(nets$Continent),]


nets$From_Database[nets$From_Database == "N/A"] <- NA
nets <- nets[!is.na(nets$From_Database),]

#organizing the necessary data
reused.web <- nets %>%
    group_by(Web_Code, Web_Year, Web_Decade, Data_Status,
             Country, From_Database,
             ISO3, Biome_WWF, Continent, Hemisphere) %>%
    summarise(ReusedCount = n())

#removing duplication
reused.web <- reused.web[!duplicated(reused.web$Web_Code), ]

#if the paper is original and never been reused the count should be 0
original <- reused.web$Data_Status %in% "Original"
reused.web$ReusedCount[original] <- 0

#checking the frequency of each network
n_occur <- data.frame(table(nets$Web_Code))

## age of each original network
reused.web$Age  <-  2022- reused.web$Web_Year

plot(reused.web$ReusedCount ~ reused.web$Age)

reused.web$ReusedCount  <- reused.web$ReusedCount - 1

hist(reused.web$ReusedCount)

reused.web$Continent <- factor(reused.web$Continent,
                               levels=c("North America",
                                        "South America",
                                        "Africa",
                                        "Europe",
                                        "Oceania", "Asia"  ))

mod.age <- glm.nb(ReusedCount ~ scale(Age)*Continent +
                      From_Database,
    data=reused.web)

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
