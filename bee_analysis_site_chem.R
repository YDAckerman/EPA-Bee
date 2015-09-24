## site - chemical analysis/visualization

library(dplyr)
library(ggplot2)
library(plyr)
library(reshape)
library(wesanderson)
library(stringdist)

## set working directory
setwd("~/Documents/ZhangLab/")

## load up site_chem
load("BeePUR/YA_bee_analysis.rda")

## get some helper functions (env hf) just in case
source("R/helper_functions.R") ## local machine

## load LD50 for pesticides
tox <- read.csv("BeePUR/AI-BeeToxicity.csv") ## local machine
tox$chem_code <- as.character(tox$chem_code)

## load crop attractiveness to bees:
load("BeePUR/cropBeeAttr.rda")

### visualize site - chemical relationship FOR 2013 ###

## now all site_chem joining is being done on abound. All
## that's needed to be done here is filter by year and
## rename/clean some variables.

## add "total LD"
site_chem <- site_chem %>%
    mutate(total_LD = 453.592 * total_lbs / (LD50..ug.bee.1. / (10^6)))

## site_chem really needs to be looked at one year
## and one buffer at a time, that way we can get some
## orders on site_code and chem_code:
site_chem_2013 <- site_chem %>%
    filter(year == 2013 & buff_size == 1) %>%
    mutate(
        chemname_w_LD50 = paste(chemname, LD50..ug.bee.1., sep = " - ")
        ) %>% filter(
            HB.Pol %in% c("++", "+") | HB.Nec %in% c("++", "+")
            )


## order site_name  by attractiveness to bees and by frequency of
## the site (see site_chem contruction above,
## in particular the construction for r [rank])

## todo: make this work without killing off things
r <- site_freq %>%
    filter(year == 2013 & buff_size == 5) %>%
    select(site_name, perc_freq) %>%
    distinct(site_name, perc_freq)

site_chem_2013 <- left_join(site_chem_2013, r, by = "site_name")

order1 <- site_chem_2013 %>%
    arrange(desc(perc_freq)) %>%
    select(site_name) %>%
    distinct(site_name)

site_chem_2013$site_name <- factor(site_chem_2013$site_name,
                                      levels = order1$site_name)

## ordered chemname by LD50
order2 <- site_chem_2013 %>%
    arrange(LD50..ug.bee.1.) %>%
    distinct(chemname, chemname_w_LD50) 
site_chem_2013$chemname_w_LD50 <- factor(site_chem_2013$chemname_w_LD50,
                                         levels = order2$chemname_w_LD50)
site_chem_2013$chemname <- factor(site_chem_2013$chemname,
                                  levels = order2$chemname)

### visualize the relationship between site and crop
### by number of total lbs applied

## this can either be visualized in num
## bin the results:
bins <- c(list(c(0.0001, 100)),list(c(101,200)), list(c(201,2000)), list(c(2001,20000)),
          list(c(20001,60500)))

site_chem_2013$binned_applications <-
    unlist(llply(site_chem_2013$total_lbs,
                 function(x){
                     if(x > 1){ x <- floor(x) }
                     i <- unlist(llply(bins, function(bin){
                         x <= max(bin) & x >= min(bin)
                     }))
                     if(length(which(i)) == 0){print(x)}
                     which(i)
                 }, .inform = TRUE))

labels <- unlist(llply(bins, function(bin){
    paste0(min(bin), " - ",max(bin))
}))
pal <- wes_palette("Zissou", length(labels), type = "continuous")

ggplot(site_chem_2013, aes(x = site_name,
                           y = chemname_w_LD50,
                           fill = as.factor(binned_applications))) +
    geom_tile() +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 9)) +
    theme(axis.text.y = element_text(angle = 0, hjust = 0, size = 8)) +
    scale_fill_manual(name = "Total Lbs Applied",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels) +
    xlab(label = "Crop \n (ordered by decresing frequency of co-occurence with almonds)") +
    ylab(label = "Active Ingredient & LD50 \n (ordered by increasing LD50)") +
    ggtitle("Total Lbs Active Ingredient Use \n Bee-Attractive Crops 2013")

### Do the same but with lethal doses (!?)

bins <- c(list(c(400, 4000000)), list(c(4000001, 30000000)),
          list(c(30000001, 100000000)),
          list(c(100000001, 500000000)),
          list(c(500000001, 1000000000)),
          list(c(1000000001, 10000000000)),
          list(c(10000000001, 300000000000)))

site_chem_2013$binned_applications <-
    unlist(llply(site_chem_2013$med_LD_acre_planted,
                 function(x){
                     is.na(x) && return(NA)
                     x <- floor(x)
                     i <- unlist(llply(bins, function(bin){
                         x <= max(bin) & x >= min(bin, na.rm = TRUE)
                     }))
                     if(length(which(i)) == 0){print(x)}
                     which(i)
                 }, .inform = TRUE))

labels <- unlist(llply(bins, function(bin){
 paste0(signif(min(bin),2), " - ",signif(max(bin),2))
}))
pal <- wes_palette("Rushmore", length(bins), type = "continuous")

ggplot(site_chem_2013, aes(x = site_name,
                           y = chemname_w_LD50,
                           fill = as.factor(binned_applications))) +
    geom_tile() +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 9)) +
    theme(axis.text.y = element_text(angle = 0, hjust = 0, size = 8)) +
    scale_fill_manual(name = "Median lethal doses per Acre Planted",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels) +
    xlab(label = "Crop \n (ordered by decresing frequency of co-occurence with almonds)") +
    ylab(label = "Active Ingredient & LD50 \n (ordered by increasing LD50)") +
    ggtitle("Average Lethal Doses per Acre Planted Active Ingredient Use \n Bee-Attractive Crops 2013")


### The same, but with products

site_prod_2013 <- site_prod %>%
    filter(year == 2013)

## order site_name  by attractiveness to bees and by frequency of
## the site (see site_chem contruction above,
## in particular the construction for r [rank])

r_prod <- site_freq %>%
    filter(year == 2013 & buff_size == 5) %>%
    dplyr::select(site_name, site_code, perc_freq) %>%
    distinct(site_name, site_code, perc_freq) %>%
    dplyr::mutate(site_code = as.numeric(site_code))

site_prod_2013 <- left_join(site_prod_2013, r_prod, by = "site_code")

order1 <- site_prod_2013 %>%
    arrange(desc(perc_freq)) %>%
    select(site_name) %>%
    distinct(site_name)

site_prod_2013$site_name <- factor(site_prod_2013$site_name,
                                      levels = order1$site_name)

## ordered chemname by LD50
order2 <- site_prod_2013 %>%
    arrange(total_lbs) %>%
    distinct(product_name) 
site_prod_2013$product_name <- factor(site_prod_2013$product_name,
                                         levels = order2$product_name)

bins <- c(list(c(0.0001,100)), list(c(100.1, 5000)), list(c(5001, 17000)),
          list(c(17001, 80000)), list(c(80001, 100000)), list(c(100001, 170000)))

site_prod_2013$binned_applications <-
    unlist(llply(site_prod_2013$total_lbs,
                 function(x){
                     is.na(x) && return(NA)
                     i <- unlist(llply(bins, function(bin){
                         x <= max(bin) & x >= min(bin, na.rm = TRUE)
                     }))
                     if(length(which(i)) == 0){print(x)}
                     which(i)
                 }, .inform = TRUE))

labels <- unlist(llply(bins, function(bin){
 paste0(signif(min(bin),2), " - ",signif(max(bin),2))
}))
pal <- wes_palette("Rushmore", length(bins), type = "continuous")



ggplot(site_prod_2013 %>% filter(total_lbs >= 5000), aes(x = site_name,
                           y = product_name,
                           fill = as.factor(binned_applications))) +
    geom_tile() +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 9)) +
    theme(axis.text.y = element_text(angle = 0, hjust = 0, size = 8)) +
    scale_fill_manual(name = "Total Lbs Applied",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels) +
    xlab(label = "Crop \n (ordered by decresing frequency of co-occurence with almonds)") +
    ylab(label = "Product Name") +
    ggtitle("Product Use by Site 2013")

prod_summary <- site_prod %>%
    group_by(year, prodno, product_name) %>%
    dplyr::summarise(total_use = sum(total_lbs, na.rm = TRUE),
                     median_use_per_crop = median(total_lbs, na.rm = TRUE),
                     average_use_per_crop = mean(total_lbs, na.rm = TRUE))


ggplot(prod_summary, aes(product_name, y = total_use, group = year, color = year)) +
    geom_point() +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 5))

### rank bee crops by usage:
site_summ_chem <- site_chem %>%
    filter(year == 2013 &
           (HB.Nec %in% c("++","+") |
           HB.Pol %in% c("++","+")) &
           buff_size == 5 &
           aer_gnd_ind == "tot") %>%
    dplyr::mutate(total_LD = total_lbs * 453.592  / (LD50..ug.bee.1. / (10^6))) %>%
    group_by(year, site_code, site_name) %>%
    dplyr::summarise(total_use_chem = sum(total_lbs, na.rm = TRUE),
                     total_LD_chem= sum(total_LD, na.rm = TRUE),
                     num_applications_chem = sum(num_applications, na.rm = TRUE)
                     )
    
site_summ_tot <- site_chem %>%
    filter(year == 2013 &
           (HB.Nec %in% c("++","+") |
           HB.Pol %in% c("++","+")) &
           buff_size == 5 &
           aer_gnd_ind == "tot") %>%
    dplyr::mutate(total_LD = total_lbs * 453.592  / (LD50..ug.bee.1. / (10^6))) %>%
    group_by(year) %>%
    dplyr::summarise(total_use = sum(total_lbs, na.rm = TRUE),
                     total_LD = sum(total_LD, na.rm = TRUE),
                     num_applications = sum(num_applications, na.rm = TRUE)
                     )
    
site_summ <- left_join(site_summ_chem, site_summ_tot, by = "year")

site_summ <- site_summ %>%
    ungroup() %>%
    dplyr::mutate(perc_use = round(total_use_chem / total_use, 3),
                  perc_LD = round(total_LD_chem / total_LD, 3),
                  perc_applications = round(num_applications_chem / num_applications, 3))



### quantify ranks of usage within bee crops:
chem_summ_chem <- site_chem %>%
    filter(year == 2013 &
           (HB.Nec %in% c("++","+") |
           HB.Pol %in% c("++","+")) &
           buff_size == 5  &
           aer_gnd_ind == "tot") %>%
    dplyr::mutate(total_LD = total_lbs * 453.592  / (LD50..ug.bee.1. / (10^6))) %>%
    group_by(year, chem_code, chemname) %>%
    dplyr::summarise(total_use_chem = sum(total_lbs, na.rm = TRUE),
                     total_LD_chem= sum(total_LD, na.rm = TRUE),
                     num_applications_chem = sum(num_applications, na.rm = TRUE)
                     )
    
chem_summ_tot <- site_chem %>%
    filter(year == 2013 &
           (HB.Nec %in% c("++","+") |
           HB.Pol %in% c("++","+")) &
           buff_size == 5 &
           aer_gnd_ind == "tot") %>%
    dplyr::mutate(total_LD = total_lbs * 453.592  / (LD50..ug.bee.1. / (10^6))) %>%
    group_by(year) %>%
    dplyr::summarise(total_use = sum(total_lbs, na.rm = TRUE),
                     total_LD = sum(total_LD, na.rm = TRUE),
                     num_applications = sum(num_applications, na.rm = TRUE)
                     )
    
chem_summ <- left_join(chem_summ_chem, chem_summ_tot, by = "year")

chem_summ <- chem_summ %>%
    ungroup() %>%
    dplyr::mutate(perc_use = round(total_use_chem / total_use, 3),
                  perc_LD = round(total_LD_chem / total_LD, 3),
                  perc_applications = round(num_applications_chem / num_applications, 3))

chem_summ <- as.data.frame(chem_summ)

## chem_summ tables
chem_summ %>%
	  arrange(desc(perc_use)) %>%
    select(chemname, fraction_total_use = perc_use,
           fraction_total_LD = perc_LD,
           fraction_all_applications = perc_applications) %>%
	  head(10)

chem_summ %>%
	  arrange(desc(perc_LD)) %>%
	  select(chemname, fraction_total_LD = perc_LD) %>%
	  head(10)

chem_summ %>%
	  arrange(desc(perc_applications)) %>%
	  select(chemname, fraction_all_applications = perc_applications) %>%
	  head(10)


tmp <- melt(chem_summ, id = "chemname")

ggplot(tmp, aes(x = chemname, y = value, color = variable))  +
    geom_point() +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 9)) 




rank_by_use <- site_chem %>%
    filter( FALSE | HB.Nec %in% c("++", "+") |
           HB.Pol %in% c("++", "+")) %>%
    filter( aer_gnd_ind == "tot" ) %>%
    group_by(year, buff_size, site_code, site_name) %>%
    dplyr::mutate(total_lds = 453.592 * total_lbs /
                  (LD50..ug.bee.1. / (10^6))) %>%
    dplyr::summarise(total_lbs = sum(total_lbs),
                     total_lds = sum(total_lds)) %>%
    ungroup() %>%
    filter(year == 2013 & buff_size == 5) %>%
    select(site_name, total_lds) %>%
    top_n(10)
