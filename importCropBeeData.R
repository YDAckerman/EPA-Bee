## set wd, load libs, source helpers, read data, and define input files
setwd("/Users/Yoni/Documents/ZhangLab/BeePUR/cropBeeData/")

## import libraries
library(xlsx)
library(plyr)
library(dplyr)

source("~/Documents/ZhangLab/R/helper_functions.R")

## get all files to import
files <- list.files(getwd(), full.names = TRUE)

## read each xlsx file and merge into one df
cropAttr <- ldply(files, function(file){
    d <- read.xlsx(file, stringsAsFactors = FALSE,
                   sheetIndex = 1)
    colnames(d)[2:4] <- c("Grp_Description", "HB.Pol", "HB.Nec")
    d %>% select(Crop, HB.Pol, HB.Nec, Grp_Description)
})

## spot corrections
cropAttr$Crop <- gsub("\n", " ", cropAttr$Crop)
cropAttr$Crop <- hf$removeParens(cropAttr$Crop)

## whoa spot corrections. easy, now.
cropAttr[, c("HB.Pol", "HB.Nec")] <-
    llply(cropAttr[, c("HB.Pol", "HB.Nec")], function(y){
    unlist(llply(y, function(x){
        x <- gsub("[0-9]", "", gsub("\n", "", x))
        ifelse(hf$mgrepl(c("\\+", "\\-"), x), x, NA)
    }))})

## change tomatoes to tomAhtoes:
cropAttr$Crop <- gsub("Tomatoes", "Tomato", cropAttr$Crop)
cropAttr$Crop <- gsub("Cherries", "Cherry", cropAttr$Crop)
cropAttr$Crop <- gsub("Blueberries", "Blueberry", cropAttr$Crop)
cropAttr$Crop <- gsub("Lemons/ limes", "Lemons", cropAttr$Crop)
cropAttr$Crop <- gsub("Strawberries", "Strawberry", cropAttr$Crop)
cropAttr$Crop <- gsub("Watermelon s", "Watermelons", cropAttr$Crop)
cropAttr$Crop <- gsub("Chick peas", "Garbanzos", cropAttr$Crop)

save(cropAttr, file = "~/Documents/ZhangLab/BeePUR/cropBeeAttr.rda")

