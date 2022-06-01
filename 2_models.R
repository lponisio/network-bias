rm(list=ls())
setwd("~/Dropbox (University of Oregon)/")
## setwd("/Volumes/bombus/Dropbox (University of Oregon)")
## setwd("\Dropbox (University of Oregon)")
## setwd("C:/Users/emanu/Dropbox (University of Oregon)")

setwd("network-bias-saved")

## dataset
load('saved/area_richness_web.Rdata')
load('saved/biome_webs.Rdata')
load('saved/GDP_web.Rdata')
load('saved/res_inv_web.Rdata')
load('saved/webs_raw.Rdata')
load('saved/webs_all_data.Rdata')
load('saved/biomes_data.Rdata')

#packages
library(performance)
library(lme4)
library(nlme)
library(MuMIn)
library(MASS)
library(car)
library(sjPlot)
library(ggplot2)
library(ggeffects)

## ***********************************************
## AREA by biome
## ***********************************************

## global
# need change variable type
biome.webs$GlobalWebs <- as.numeric(biome.webs$GlobalWebs)

biome.area.mod <- glm(GlobalWebs ~ log(GlobalArea),
                      data=biome.webs, family = "poisson")
plot(biome.area.mod)
summary(biome.area.mod)

ggplot(biome.webs, aes(x = GlobalArea, y = GlobalWebs)) +
  geom_point()

summary(biome.area.mod)

r.squaredGLMM(biome.area.mod)

check_model(biome.area.mod)



## N hemiphere
biome.area.mod.N <- glm(NorthernWebs ~ log(NorthernArea),
                      data=biome.webs, family="poisson")

summary(biome.area.mod.N)
check_model(biome.area.mod.N)
plot(biome.area.mod.N)


## S hemisphere
biome.area.mod.S <- glm(SouthernWebs ~ log(SouthernArea),
                      data=biome.webs, family="poisson")

summary(biome.area.mod.S)

#grouping a model with all biome data

biome.net.m1 <- glm(Webs ~ log(Area), data = biomes_web_data[1:28,], family = "poisson")
summary(biome.net.m1)
plot(biome.net.m2)

biome.net.m2 <- glm(Webs ~ log(Area) + Hemisphere, data = biomes_web_data[1:28,],
                  family = "poisson")
summary(biome.net.m2)
plot(biome.net.m2)


ggplot(biomes_web_data[1:28,], aes(x = log(Area), y = Webs, color = Hemisphere) ) +
  geom_point() +
  geom_smooth(method = "glm", se = T)


## ***********************************************
## GLMM Models
## ***********************************************

#GDP x Area x Species
gdp_area_species <- na.omit(gdp_area_species)

country.mod <- glmer(Web.count ~
                       log(ResInvestTotal)+
                       log(AREA)+
                       log(CL_Species)+
                       (1|Hemisphere)+
                       (1|Continent),
                       data=gdp_area_species, family = "poisson")

summary(country.mod)

r.squaredGLMM(country.mod)

plot_model(country.mod)
tab_model(country.mod)

pr <- ggpredict(country.mod, c("Continent", "Hemisphere"))
plot(pr)


ggplot(gdp_area_species, aes(x = AREA, y = Web.count, color = Continent) ) +
  geom_point() +
  geom_smooth(method = "lm", se = T)

ggplot(gdp_area_species, aes(x = ResInvestTotal, y = Web.count, color = Hemisphere) ) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

gdp_area_species$fit <- predict(country.mod)

ggplot(gdp_area_species, aes(x = log(AREA), y = Web.count, color = Continent)) +
  geom_line(aes(y=fit, lty=Continent), size=0.8) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw()



ggplot(gdp_area_species, aes(log(AREA), Web.count, col = Continent)) +
  geom_point() +
  geom_line(aes(y = fit), size = 1)

