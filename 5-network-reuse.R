rm(list=ls())
library(tidyverse)

## This script calculates the area for each biome globally and in the
## N and S hemisphere

setwd("~/Dropbox (University of Oregon)/")
## setwd("/Volumes/bombus/Dropbox (University of Oregon)")
## setwd("\Dropbox (University of Oregon)")
## setwd("C:/Users/emanu/Dropbox (University of Oregon)")

setwd("network-bias-saved")

## nets <- read.csv("network_reuse.csv")

## save(nets, file="network_reuse.Rdata")

load(file="network_reuse.Rdata")

reused.web <- nets %>%
    group_by(Web_Code, Web_Year, Country, Biome_WWF, Continent) %>%
    summarise(ReusedCount = n())

## age of each original network
reused.web$Age  <-  2022- reused.web$Web_Year

plot(reused.web$ReusedCount ~ reused.web$Age)

mod.age <- glm(ReusedCount ~ Age  + Continent, family ="poisson",
    data=reused.web)


summary(mod.age)



pframe <- with(reused.web,
             expand.grid(Age=seq(min(Age),max(Age),length=51),
                         Continent=unique(Continent)))

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
