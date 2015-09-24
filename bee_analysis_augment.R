library(dplyr)
library(ggplot2)
library(plyr)
library(reshape)
library(wesanderson)
library(stringdist)
library(gridExtra)
library(data.table)


## set working directory
setwd("~/Documents/ZhangLab/")

## load up yearlies, pur_site, and pur_chem
load("BeePUR/YA_bee_analysis.rda")

## get some helper functions (env hf) just in case
source("R/helper_functions.R") ## local machine

## load LD50 for pesticides
tox <- read.csv("BeePUR/AI-BeeToxicity.csv")
tox$chem_code <- as.character(tox$chem_code)

## load crop attractiveness to bees:
load("BeePUR/cropBeeAttr.rda")

## read in halflife data:
halfLives <- read.csv("BeePUR/pesticide_half_life.csv",
                      stringsAsFactors = FALSE) %>%
    dplyr::rename(chem_code = chemical_code,
           chemname = chemical_name)

## load use type info:
load("~/Documents/ZhangLab/BeePUR/ai_use_type.rda")

## summarize ais:
ais_info <- AI_ranks_all_chem %>%
    ungroup() %>%
    select(chem_code, chemname) %>%
    distinct(chem_code, chemname) %>%
    mutate(chem_code = as.numeric(chem_code))

ais_info <- left_join(ais_info, ai_use_type, by = c("chem_code", "chemname"))
ais_info <- left_join(ais_info, halfLives, by = c("chem_code", "chemname"))

