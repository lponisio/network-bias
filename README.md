# network-bias

Manuscript: Geographic biases and gaps in the sampling of plant--pollinator networks

README for analysis code

## Overview

This repository contains the code and data-processing pipeline used in the manuscript "Geographic biases and gaps in the sampling of plant--pollinator networks." The workflow use multiple country-level datasets, constructs analysis-ready variables, fits statistical models to explain spatial patterns in network sampling and reuse, and produces the figures and tables used in the manuscript.

## Raw datasets

All input files are placed in 'data/raw/'

webs.csv ( ; ) -- Plant--pollinator network metadata.

network_reuse.csv ( ; ) -- Records of reuse/citations of networks.

research_expenditure.csv ( , ) -- World Bank R&D expenditure (% of GDP), series GB.XPD.RSDV.GD.ZS.

gdp.csv ( , ) -- World Bank GDP (current US\$), series NY.GDP.MKTP.CD.

bees_by_country.csv ( , ) -- From Orr et al. (2021): national area (km²) and bee species richness.

UNSD_continents.csv ( ; ) -- UN Statistics Division mapping of countries to regions/continents.

## Workflow summary

## Initialization (src/) Run these in order to set paths, load packages, prepare data, fit models, and build figures.

### 1) Paths & output locations

source("src/lab_paths.R")

### 2) Required packages

source("src/initialize_packages.R")

### 3) Data: load, standardize, merge (writes webs_complete.csv)

source("src/initalize_data.R")

### 4) Models: fit and export LaTeX tables

source("src/initalize_models.R")

### 5) Figures: predictions, maps, panels (saves manuscript figures)

source("src/initalize_figure.R")

## Macro pipeline (consolidated scripts): After the src step completes, run the end-to-end pipeline:

### 1) Data preparation (wraps/organizes the data prep stage)

source("1_dataPrep.R")

### 2) Modeling (NB-GLM for country counts; LMM for reuse)

source("2_models.R")

### 3) Figures (prediction curves, maps, patchwork panels)

source("3_figures.R")

## Citation & data acknowledgments

World Bank Open Data: GDP and R&D expenditure.

Orr, M. C., et al. (2021). Global Patterns of Bee Diversity. Current Biology. <https://doi.org/10.1016/j.cub.2020.10.053>

UN Statistics Division: country/region classifications.
