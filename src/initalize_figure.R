
#packages
library(performance)
library(lme4)
library(dplyr)
library(MASS)
library(cowplot)
library(ggplot2)
## library(ggeffects)
library(viridis)
library(ggplot2)
library(tidyverse)
library(maps)
library(grid) # For unit()

# Load required libraries
library(rnaturalearth)  # for world map data
library(sf)             # for spatial data manipulation
library(cartogram)      # for generating cartograms
library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)
#data
webs <- read.csv("network-bias-saved/saved/webs_complete.csv")
source("network-bias/2_models.R")
savefilepath <- c("network-bias-saved/manuscript/figures")
## ***********************************************
