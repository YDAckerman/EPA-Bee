---
title: "BeePUR Results"
author: "Yoni Ackerman"
output: html_document
---

Background
--------------------------------------------------------------------------------

```{r, echo = FALSE, message = FALSE}

library(dplyr)
library(ggplot2)
library(plyr)
library(reshape2)
library(wesanderson)
library(data.table)
library(stringdist)
library(gridExtra)
library(shiny)


load("~/Dropbox/ZhangLabData/BeePUR/YA_bee_analysis.rda")
load("~/Dropbox/ZhangLabData/BeePUR/yearly_sums.rda")
load("~/Dropbox/ZhangLabData/BeePUR/ais_info.rda")
load("~/Dropbox/ZhangLabData/BeePUR/site_frequencies.rda")
load("~/Dropbox/ZhangLabData/BeePUR/data.rda")
load("~/Dropbox/ZhangLabData/BeePUR/comtrs_data.rda")
load("~/Dropbox/ZhangLabData/BeePUR/yearlyAlmondTrends.rda")

cnty_hl_dat <- as.data.frame(dat)
comtrs_hl_dat <- as.data.frame(data)


## change codes to characters:
pur_site$site_code <- as.character(pur_site$site_code)
pur_chem$chem_code <- as.character(pur_chem$chem_code)
yearly_chem_by_crop$site_code <- as.character(yearly_chem_by_crop$site_code)

## get some helper functions (env hf) just in case
source("~/Documents/Coding/R/R_convenience/helper_functions.R") ## local machine

## load LD50 for pesticides
tox <- read.csv("~/Dropbox/ZhangLabData/BeePUR/AI-BeeToxicity.csv") ## local machine
tox$chem_code <- as.character(tox$chem_code)

## load crop attractiveness to bees:
load("~/Dropbox/ZhangLabData/BeePUR/cropBeeAttr.rda")
```

* Data spans the 23 year period between 1991 and 2013.

* We focus on the 56 active ingredients currently under regulatory scrutiny and their usage within buffers of 1-5 miles of 5954 bearing almond sections across California.

* In addition we examine the distribution of ai use across 182 different crop (site) types found in California, giving particular attention to those [known to be attractive to bees](http://www.ree.usda.gov/ree/news/Attractiveness_of_Agriculture_crops_to_pollinating_bees_Report-FINAL.pdf). 

## Crop/Site - AI Stats and Trends

* From 1991 to 2013 active ingredient use in bee-attractive crops has fluctuated between 95% to 86% of the total use across all site types.

* In 2013, 86% of ai use within 1 mi of an almond section was applied to bee-attractive crops; within 5 mi of an almond section, 87% of ai usage was applied to bee-attractive crops.

```{r, echo = FALSE}
### visualize the percentage of use in bee crops:
bee_crop_use_summary <- perc_chem_use_by_site %>%
    group_by(year, buff_size) %>%
    filter(HB.Nec %in% c("++","+") | HB.Pol %in% c("++","+")) %>%
    dplyr::summarise(total_perc = sum(percent_use))
```

```{r, echo = FALSE, fig.width = 16, fig.height = 10}
ggplot(almond_dat, aes(x = year, y = value, group = variable)) +
    geom_line() +
    facet_wrap(~variable, scale = "free") +
    ggtitle("Chemical Presence in Bearing Almond Sections")
```

```{r, echo = FALSE, fig.width = 16}
g1 <- ggplot(bee_crop_use_summary, aes(x = year, y = total_perc,
                                 group = as.factor(buff_size),
                                 color = as.factor(buff_size))) +
      geom_point() +
      geom_line() +
      coord_cartesian(ylim = c(0,1)) +
      scale_color_manual(values = wes_palette("Zissou", 5, type = "continuous"),
                         name = "Buffer Size (Mi)") +
      labs(title = "Proportion of Active Ingredient use in Bee-Attractive Crops",
           ##x = "Year",
	   y = "Proportion") +
      theme(plot.title = element_text(size = 20),
            axis.title.x = element_blank(),
	    axis.title.y = element_text(size = 13))
      ## xlab("Year") +
      ## ylab("Proportion") +
      ## ggtitle("Percent of Active Ingredient use in Bee-Attractive Crops")

g2 <- ggplot(bee_crop_use_summary, aes(x = year, y = total_perc,
                                 group = as.factor(buff_size),
                                 color = as.factor(buff_size))) +
      geom_point() +
      geom_line() +
      scale_color_manual(values = wes_palette("Zissou", 5, type = "continuous"),
                         name = "Buffer Size (Mi)") +
      labs(x = "Year",
	   y = "Proportion") +
      theme(axis.title.x = element_text(size = 13),
	    axis.title.y = element_text(size = 13))

grid.arrange(arrangeGrob(g1, ncol = 1), arrangeGrob(g2, ncol = 1))

g <- arrangeGrob(arrangeGrob(g1, ncol = 1), arrangeGrob(g2, ncol = 1))
```

The following tables ranks active ingredients by their total lbs applied, total lethal doses (determined using LD50) applied, and total number of applications made to/on bee-attractive crops. Proportions were found by dividing by the totals of all active ingredients applied to bee-attractive crops within a 5 mile buffer of bearing almond sections in 2013:
 
...removed till publication :-(


The following tables ranks bee-attractive crops according to how much of the three quantities (lbs of active ingredient, lethal doses, and applications) they contribute to the total amount across applied to bee-attractive crops within 5 mi of a bearing almond section in 2013:

...also removed :-((

***

As seen above, our picture of AI ranks changes sharply when we look from total lbs applied to total lethal doses applied.

## Distribution of AI use across (bee-attractive) site types
 
```{r, echo = FALSE}
site_chem_2013 <- site_chem %>%
    filter(year == 2013 & buff_size == 1 & aer_gnd_ind == "tot") %>%
    mutate(
        chemname_w_LD50 = paste(chemname, LD50..ug.bee.1., sep = " - ")
        ) %>% filter(
            HB.Pol %in% c("++", "+") | HB.Nec %in% c("++", "+")
            )
## order site_name  by attractiveness to bees and by frequency of
## the site (see site_chem contruction above,
## in particular the construction for r [rank])
## todo: make this work without killing off things
## r <- site_freq %>%
##     filter(year == 2013 & buff_size == 5) %>%
##     select(site_name, perc_freq) %>%
##     distinct(site_name, perc_freq)
## site_chem_2013 <- left_join(site_chem_2013, r, by = "site_name")
## order1 <- site_chem_2013 %>%
##     arrange(desc(perc_freq)) %>%
##     select(site_name) %>%
##     distinct(site_name)

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

order1 <- site_summ %>%
          arrange(desc(total_LD_chem)) %>%
	  dplyr::select(site_name, site_code)

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
tmp <- ais_info %>%
    select(-chemname) %>%
    mutate(chem_code = as.character(chem_code))

site_chem_2013 <- left_join(site_chem_2013, tmp, by = c("chem_code"))

tmp <- ais_info %>%
    mutate(aerobic_half_life_in_soil = as.numeric(aerobic_half_life_in_soil),
           anaerobic_half_life_in_soil = as.numeric(anaerobic_half_life_in_soil),
           half_life_in_water = as.numeric(half_life_in_water)) %>%
    filter(aerobic_half_life_in_soil >= 60 | anaerobic_half_life_in_soil >= 60 | half_life_in_water >= 60) %>%
    mutate(chem_code = as.character(chem_code)) %>%
    select(chem_code)

```

#### Distribution of Total Lbs Applied

```{r, echo = FALSE}
bins <- c(list(c(0.0001, 100)),
          list(c(101,200)),
	  list(c(201,2000)),
	  list(c(2001,20000)),
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

site_chem_2013$binned_applications <- factor(site_chem_2013$binned_applications, levels = sort(unique(site_chem_2013$binned_applications)))
```

```{r, echo = FALSE, fig.width = 16, fig.height = 14}
ggplot(site_chem_2013, aes(x = site_name,
                           y = chemname_w_LD50,
                           fill = binned_applications)) +
    geom_tile() +			
    scale_fill_manual(name = "Total Lbs \n Applied",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels,
		      drop = FALSE) +
    labs(title = "Total Lbs Active Ingredient Presence by Crop and AI \n for 5 mi buffered, Bee-Attractive Crops in 2013",
           x = "Crop \n (ordered by decreasing rank of LD contribution)",
	   y = "Active Ingredient & LD50 \n (ordered by increasing LD50)") +
    theme(plot.title = element_text(size = 16),
          axis.title.x = element_text(size = 11),
	  axis.title.y = element_text(size = 11),
          axis.text.x = element_text(angle = 90, hjust = 0, size = 5),
	  axis.text.y = element_text(angle = 0, hjust = 0, size = 7)##,
	  ##legend.position = c(-.08,-.48),
	  ##legend.key.size = unit(.5, "cm")
	  )

```
```{r, echo = FALSE, fig.width = 16, fig.height = 5}
## Neonics
ggplot(site_chem_2013 %>% filter(ai_class == "NEONICOTINOID"), aes(x = site_name,
                           y = chemname_w_LD50,
                           fill = binned_applications)) +
    geom_tile() +
    scale_fill_manual(name = "Total Lbs \n Applied",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels,
		      drop = FALSE) +
    labs(title = "Total Lbs Active Ingredient Presence by Crop and AI (neonics) \n for 5 mi buffered, Bee-Attractive Crops in 2013",
           x = "Crop \n (ordered by decreasing rank of LD contribution)",
	   y = "Active Ingredient & LD50 \n (ordered by increasing LD50)") +
    theme(plot.title = element_text(size = 16),
          axis.title.x = element_text(size = 11),
	  axis.title.y = element_text(size = 11),
          axis.text.x = element_text(angle = 90, hjust = 0, size = 7),
	  axis.text.y = element_text(angle = 0, hjust = 0, size = 9)##,
	  ##legend.position = c(-.08,-.48),
	  ##legend.key.size = unit(.5, "cm")
	  )
```
```{r, echo = FALSE, fig.width = 16, fig.height = 6}
## Organophosphate
ggplot(site_chem_2013 %>% filter(ai_class == "ORGANOPHOSPHATE"), aes(x = site_name,
                           y = chemname_w_LD50,
                           fill = binned_applications)) +
    geom_tile() +
    scale_fill_manual(name = "Total Lbs \n Applied",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels,
		      drop = FALSE) +
    labs(title = "Total Lbs Active Ingredient Presence by Crop and AI (organophosphates) \n for 5 mi buffered, Bee-Attractive Crops in 2013",
           x = "Crop \n (ordered by decreasing rank of LD contribution)",
	   y = "Active Ingredient & LD50 \n (ordered by increasing LD50)") +
    theme(plot.title = element_text(size = 16),
          axis.title.x = element_text(size = 11),
	  axis.title.y = element_text(size = 11),
          axis.text.x = element_text(angle = 90, hjust = 0, size = 7),
	  axis.text.y = element_text(angle = 0, hjust = 0, size = 9)##,
	  ##legend.position = c(-.08,-.48),
	  ##legend.key.size = unit(.5, "cm")
	  )

```
```{r, echo = FALSE, fig.width = 16, fig.height = 8}
## Pyrethroid
ggplot(site_chem_2013 %>% filter(ai_class == "PYRETHROID"), aes(x = site_name,
                           y = chemname_w_LD50,
                           fill = binned_applications)) +
    geom_tile() +
    scale_fill_manual(name = "Total Lbs \n Applied",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels,
		      drop = FALSE) +
    labs(title = "Total Lbs Active Ingredient Presence by Crop and AI (pyrethroids) \n for 5 mi buffered, Bee-Attractive Crops in 2013",
           x = "Crop \n (ordered by decreasing rank of LD contribution)",
	   y = "Active Ingredient & LD50 \n (ordered by increasing LD50)") +
    theme(plot.title = element_text(size = 16),
          axis.title.x = element_text(size = 11),
	  axis.title.y = element_text(size = 11),
          axis.text.x = element_text(angle = 90, hjust = 0, size = 7),
	  axis.text.y = element_text(angle = 0, hjust = 0, size = 9)##,
	  ##legend.position = c(-.08,-.48),
	  ##legend.key.size = unit(.5, "cm")
	  )

```
```{r, echo = FALSE, fig.width = 16, fig.height = 10}
## half life
ggplot(site_chem_2013 %>% filter(chem_code %in% tmp$chem_code), aes(x = site_name,
                           y = chemname_w_LD50,
                           fill = binned_applications)) +
    geom_tile() +
    labs(title = "Total Lbs AI Presence by Crop and AI (half life >= 60) \n for 5 mi buffered, Bee-Attractive Crops in 2013",
           x = "Crop \n (ordered by decreasing rank of LD contribution)",
	   y = "Active Ingredient & LD50 \n (ordered by increasing LD50)") +
    theme(plot.title = element_text(size = 16),
          axis.text.x = element_text(angle = 90, hjust = 0, size = 7),
	  axis.text.y = element_text(angle = 0, hjust = 0, size = 9)##,
	  ##legend.position = c(-.08,-.38),
	  ##legend.key.size = unit(.3, "cm")
	  ) +
    scale_fill_manual(name = "Total \n Lethal Doses",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels,
		      drop = FALSE)
```


```{r, echo = FALSE}
bins <- c(list(c(2000, 4000000)), list(c(4000001, 30000000)),
          list(c(30000001, 100000000)),
          list(c(100000001, 500000000)),
          list(c(500000001, 1000000000)),
          list(c(1000000001, 10000000000)),
          list(c(10000000001, 100000000000)),
	  list(c(1e+11 + 1, 3e+11)),
	  list(c(3e+11 + 1, 1e+12)),
	  list(c(1e+12 + 1, 1e+13)),
	  list(c(1e+13 + 1, 7e+14))
	  )

site_chem_2013$binned_applications <-
    unlist(llply(site_chem_2013$total_lds,
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

site_chem_2013$binned_applications <- factor(site_chem_2013$binned_applications, levels = sort(unique(site_chem_2013$binned_applications)))
```

***

#### Distribution of Total Lethal Doses

```{r, echo = FALSE, fig.width = 16, fig.height = 14}
ggplot(site_chem_2013, aes(x = site_name,
                           y = chemname_w_LD50,
                           fill = binned_applications)) +
    geom_tile() +
    labs(title = "Total Lethal Doses Presence by Crop and AI \n for 5 mi buffered, Bee-Attractive Crops in 2013",
           x = "Crop \n (ordered by decreasing rank of LD contribution)",
	   y = "Active Ingredient & LD50 \n (ordered by increasing LD50)") +
    theme(plot.title = element_text(size = 16),
          axis.text.x = element_text(angle = 90, hjust = 0, size = 7),
	  axis.text.y = element_text(angle = 0, hjust = 0, size = 9)##,
	  ##legend.position = c(-.08,-.38),
	  ##legend.key.size = unit(.3, "cm")
	  ) +
    scale_fill_manual(name = "Total \n Lethal Doses",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels,
		      drop = FALSE)

```
```{r, echo = FALSE, fig.width = 16, fig.height = 5}
ggplot(site_chem_2013 %>% filter(ai_class == "NEONICOTINOID"), aes(x = site_name,
                           y = chemname_w_LD50,
                           fill = binned_applications)) +
    geom_tile() +
    scale_fill_manual(name = "Total Lbs \n Applied",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels,
		      drop = FALSE) +
    labs(title = "Total Lethal Dose Presence by Crop and AI (neonics) \n for 5 mi buffered, Bee-Attractive Crops in 2013",
           x = "Crop \n (ordered by decreasing rank of LD contribution)",
	   y = "Active Ingredient & LD50 \n (ordered by increasing LD50)") +
    theme(plot.title = element_text(size = 16),
          axis.title.x = element_text(size = 11),
	  axis.title.y = element_text(size = 11),
          axis.text.x = element_text(angle = 90, hjust = 0, size = 7),
	  axis.text.y = element_text(angle = 0, hjust = 0, size = 9)##,
	  ##legend.position = c(-.08,-.48),
	  ##legend.key.size = unit(.5, "cm")
	  )

```
```{r, echo = FALSE, fig.width = 16, fig.height = 7}
## Organophosphate
ggplot(site_chem_2013 %>% filter(ai_class == "ORGANOPHOSPHATE"), aes(x = site_name,
                           y = chemname_w_LD50,
                           fill = binned_applications)) +
    geom_tile() +
    scale_fill_manual(name = "Total Lbs \n Applied",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels,
		      drop = FALSE) +
    labs(title = "Total Lethal Dose Presence by Crop and AI (organophosphates) \n for 5 mi buffered, Bee-Attractive Crops in 2013",
           x = "Crop \n (ordered by decreasing rank of LD contribution)",
	   y = "Active Ingredient & LD50 \n (ordered by increasing LD50)") +
    theme(plot.title = element_text(size = 16),
          axis.title.x = element_text(size = 11),
	  axis.title.y = element_text(size = 11),
          axis.text.x = element_text(angle = 90, hjust = 0, size = 7),
	  axis.text.y = element_text(angle = 0, hjust = 0, size = 9)##,
	  ##legend.position = c(-.08,-.48),
	  ##legend.key.size = unit(.5, "cm")
	  )
```
```{r, echo = FALSE, fig.width = 16, fig.height = 10}
## Pyrethroid
ggplot(site_chem_2013 %>% filter(ai_class == "PYRETHROID"), aes(x = site_name,
                           y = chemname_w_LD50,
                           fill = binned_applications)) +
    geom_tile() +
    scale_fill_manual(name = "Total Lbs \n Applied",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels,
		      drop = FALSE) +
    labs(title = "Total Lethal Dose Presence by Crop and AI (pyrethroids) \n for 5 mi buffered, Bee-Attractive Crops in 2013",
           x = "Crop \n (ordered by decreasing rank of LD contribution)",
	   y = "Active Ingredient & LD50 \n (ordered by increasing LD50)") +
    theme(plot.title = element_text(size = 16),
          axis.title.x = element_text(size = 11),
	  axis.title.y = element_text(size = 11),
          axis.text.x = element_text(angle = 90, hjust = 0, size = 7),
	  axis.text.y = element_text(angle = 0, hjust = 0, size = 9)##,
	  ##legend.position = c(-.08,-.48),
	  ##legend.key.size = unit(.5, "cm")
	  )

```
```{r, echo = FALSE, fig.width = 16, fig.height = 14}
## Half life >= 60
ggplot(site_chem_2013 %>% filter(chem_code %in% tmp$chem_code), aes(x = site_name,
                           y = chemname_w_LD50,
                           fill = binned_applications)) +
    geom_tile() +
    labs(title = "Total Lethal Doses Presence by Crop and AI (half life >= 60) \n for 5 mi buffered, Bee-Attractive Crops in 2013",
           x = "Crop \n (ordered by decreasing rank of LD contribution)",
	   y = "Active Ingredient & LD50 \n (ordered by increasing LD50)") +
    theme(plot.title = element_text(size = 16),
          axis.text.x = element_text(angle = 90, hjust = 0, size = 7),
	  axis.text.y = element_text(angle = 0, hjust = 0, size = 9)##,
	  ##legend.position = c(-.08,-.38),
	  ##legend.key.size = unit(.3, "cm")
	  ) +
    scale_fill_manual(name = "Total \n Lethal Doses",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels,
		      drop = FALSE)
```

Almond section proximity to different bee-attractive crops and different AI's
--------------------------------------------------------------------------------

```{r, echo = FALSE, warning = FALSE}
## The following plots visualize the percent of bearing almond sections that were within the indicated distance of unique, non-almond, bee-attractive crops (that received an application of one of the 56 AI's). The percentages have been rounded to the nearest tenth for plotting purposes.

## bins <- c(list(0), list(1:5), list(6:10), list(11:20), list(21:29))
          
## det_bins <- function(.d){
##     ldply(bins, function(bin){
##         tmp <- .d %>%
##              filter( (num_bee_crops <= max(bin) & num_bee_crops >= min(bin)))
##         data.frame(bin = paste0(min(bin), "-", max(bin)),
##                    num_comtrs = length(unique(tmp$comtrs)),
##                    tot_comtrs = length(unique(.d$comtrs)),
##                    stringsAsFactors = FALSE)
##     })
## }
## d1 <- count_proximity_bee_crops %>%
##     group_by(year, buff_size) %>%
##     do(det_bins(.)) %>%
##     mutate(perc_comtrs = num_comtrs / tot_comtrs)

## g <- llply(1:5, function(buff){
##     d11 <- d1 %>% filter(buff_size == buff)
##     d11$bin <- factor(d11$bin, levels = unique(d11$bin))
##     d11$rnd_perc <- as.factor(round(d11$perc_comtrs,1))
##     ggplot(d11, aes( x = year, y = bin)) +
##         geom_tile(aes(fill = rnd_perc)) +
##             scale_fill_manual(values = wes_palette("Zissou", length(unique(d11$rnd_perc)), "continuous"),
##                               name = "Proportion of \n Almond Sites") +
##                                   ylab("Number of \n Bee-Attractive Crops") +
##                                       ggtitle(paste0(buff, " Mi Buffer"))
## })
```

```{r, fig.width = 20, fig.height = 10, echo = FALSE}
## grid.arrange(arrangeGrob(g[[2]], g[[3]], g[[4]], ncol = 3), arrangeGrob(g[[1]], g[[5]], ncol = 2))
## g <- arrangeGrob(arrangeGrob(g[[2]], g[[3]], g[[4]], ncol = 3), arrangeGrob(g[[1]], g[[5]], ncol = 2))
```

The following plot shows the 2013 distribution of number of bee-attractive crops within the stated distance
of a bearing almond section:

```{r, echo = FALSE, fig.width = 16}
count_proximity_bee_crops$buff_size <- as.factor(count_proximity_bee_crops$buff_size)
levels(count_proximity_bee_crops$buff_size) <- paste0(1:5, " Mi")
ggplot(count_proximity_bee_crops %>% filter(year == 2013), aes(x = num_bee_crops)) +
    geom_bar(aes(y = (..count..)/4427)) +
    facet_grid(~buff_size) +
    ylab("Proportion of all Almond Sections") +
    xlab("Number of unique Bee-Attractive Crops Present in Buffer") +
    ggtitle("Distributions of Number of Unique, Non-Almond, Bee-Attractive Crops \n by Buffer size in 2013")
```

The following two plots show a different distribution: the number of different active ingredients used on bee-attractive
crops within the stated distance of a bearing almond section. 

```{r, echo = FALSE, fig.width = 16}
## histogram of distribution of number of chemicals used (on bee crop) within 1-5 mile buffer
## of bearing almond sections
dd <- count_proximity_bee_crops %>%
    filter(year == 2013) %>%
    group_by(buff_size, num_chems_used) %>%
    dplyr::summarise(proportion = n() / 4427) %>%
    ungroup()

dd$buff_size <- as.factor(dd$buff_size)
levels(dd$buff_size) <- paste0(1:5, " Mi")

ggplot(dd, aes(x = num_chems_used, y = proportion)) +
    geom_bar(stat = "identity") +
    facet_grid(~buff_size) +
    ylab("Proportion of all Almond Sections") +
    xlab("Number of unique Chemicals Used in Buffer") +
    ggtitle("Distributions of Number of Unique Chemicals Used  \n on Bee-Attractive Crops by Buffer size in 2013")
```

```{r, echo = FALSE, fig.width = 16}
## histogram of distribution of number of chemicals used (on all crop) within 1-5 mile buffer
## of bearing almond sections
dd <- count_proximity_all_crops %>%
    filter(year == 2013) %>%
    group_by(buff_size, num_chems_used) %>%
    dplyr::summarise(proportion = n() / 4427) %>%
    ungroup()

dd$buff_size <- as.factor(dd$buff_size)
levels(dd$buff_size) <- paste0(1:5, " Mi")

ggplot(dd, aes(x = num_chems_used, y = proportion)) +
    geom_bar(stat = "identity") +
    facet_grid(~buff_size) +
    ylab("Proportion of all Almond Sections") +
    xlab("Number of unique Chemicals Used in Buffer") +
    ggtitle("Distributions of Number of Unique Chemicals Used \n on All Crops by Buffer size in 2013")

## ALSO, this is not bad to have on hand:
## tmp %>% group_by(buff_size) %>% dplyr::summarise(average = mean(num_chems_used), max = max(num_chems_used), median = median(num_chems_used), mode = names(sort(-table(num_chems_used)))[1])
```

In this last plot, we show, for 2013, the number of bearing almond sections that come within 3 miles of
varying numbers of bee-attractive crops and active ingredients.

```{r, echo = FALSE}
bf_size = "3 Mi"
dd <- count_proximity_bee_crops %>%
      filter(year == 2013 & buff_size == bf_size) %>%
	 group_by(num_bee_crops, num_chems_used) %>%
	 dplyr::summarise(count = n())

total <- sum(dd$count, na.rm = TRUE)
ggplot(dd, aes(x = num_bee_crops, y = num_chems_used, fill = count)) +
       geom_tile() +
       ylab("Number of AI") +
       xlab("Number of Bee-Attractive Crops") +
       ggtitle(paste("Number of Bearing Almond Sections (out of ", total,") \n within ", bf_size," of Different Bee-Crops and Different AI's", 
       sep = ""))
```


These statistics are important in light of what we know about bee foraging habits - both in
terms of spatial extent and crop diversity. They suggest, indirectly, both the likelihood that
bees will be foraging outside of their almond section, and the number of active ingredients they
could potentially encounter. (We could have a grid-type visualization of this, too...)

Yearly Trends in Usage
--------------------------------------------------------------------------------

Here we map trends in active ingredient use and presence of lethal doses across
bearing almond sections over the years 1991-2013. We use two quantities to
capture those trends: total lbs used and total lethal doses applied. 

```{r, echo = FALSE, warning = FALSE, fig.width = 16, fig.height = 8}
yearly_sums$buff_size <- as.factor(yearly_sums$buff_size)
levels(yearly_sums$buff_size) <- paste0(1:5, " Mi")
yearly_sums$variable <- as.factor(yearly_sums$variable)
levels(yearly_sums$variable) <- c("Total lbs AI", "Total Lethal Doses", "Median lbs", "Median lds", "Mean lbs", "Mean lds")
ggplot(yearly_sums %>% filter(variable %in% c("Total lbs AI", "Total Lethal Doses")),
       aes(x = year, y = value,
           group = variable, color = variable)) +
    scale_color_manual(values = wes_palette("Cavalcanti")) +
    stat_smooth(se = FALSE) +
    ylab(label = "Lethal Doses (from LD50)                              Lbs Active Ingredient") +
    facet_grid(variable ~ buff_size, scale = "free") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    xlab(label = "Year") +
    ggtitle("Yearly Trends in Total Lbs AI & Total Lethal Doses (from LD50)")
```

```{r, echo = FALSE, warning = FALSE, fig.width = 16, fig.height = 8}
yearly_sums_ai_class$buff_size <- as.factor(yearly_sums_ai_class$buff_size)
levels(yearly_sums_ai_class$buff_size) <- paste0(1:5, " Mi")
yearly_sums_ai_class$variable <- as.factor(yearly_sums_ai_class$variable)
levels(yearly_sums_ai_class$variable) <- c("Total lbs AI", "Total Lethal Doses", "Median lbs", "Median lds", "Mean lbs", "Mean lds")

yearly_sums_AGO_ai_class$buff_size <- as.factor(yearly_sums_AGO_ai_class$buff_size)
levels(yearly_sums_AGO_ai_class$buff_size) <- paste0(1:5, " Mi")
yearly_sums_AGO_ai_class$variable <- as.factor(yearly_sums_AGO_ai_class$variable)
levels(yearly_sums_AGO_ai_class$variable) <- c("Total lbs AI", "Total Lethal Doses", "Median lbs", "Median lds", "Mean lbs", "Mean lds")

ggplot(yearly_sums_ai_class %>% filter(variable %in% c("Total lbs AI", "Total Lethal Doses")),
	aes(x = year, y = value, group = ai_class, color = ai_class)) +
    	stat_smooth(se = FALSE) +
    	facet_grid(variable ~ buff_size, scale = "free") +
	theme(axis.text.x = element_text(angle = 90, hjust = 0)) +		
    	ggtitle("Yearly Trends grouped by statistic, buffer size, and AI class")

tmp = yearly_sums_AGO_ai_class %>% filter(buff_size == "5 Mi" & variable %in% c("Total lbs AI", "Total Lethal Doses"))
ggplot(tmp, aes(x = year, y = value, group = ai_class, color = ai_class)) +
    stat_smooth(se = FALSE) +
    facet_grid(variable ~ aer_gnd_ind, scale = "free") +
    ggtitle("Yearly Trends grouped by statistic, mode of application, and AI class")
```

#### Trends Across Individual Crops

We now do a similar analysis, but for the crops that appeared
most frequently within 5 miles of a bearing almond section in 2013:

```{r, echo = FALSE, warning = FALSE, fig.width = 18, fig.height = 12}

top_ten <- order1 %>% head(10)

yearly_sums_by_site$buff_size <- as.factor(yearly_sums_by_site$buff_size)
levels(yearly_sums_by_site$buff_size) <- paste0(1:5, " Mi")
yearly_sums_by_site$variable <- as.factor(yearly_sums_by_site$variable)
levels(yearly_sums_by_site$variable) <- c("Total lbs AI", "Total Lethal Doses", "Median lbs", "Median lds", "Mean lbs", "Mean lds")

ggplot(yearly_sums_by_site %>%
       filter(site_code %in% as.numeric(top_ten$site_code) &
       variable %in% c("Total lbs AI", "Total Lethal Doses")),
       aes(x = year, y = value, group = site_name, color = hf$removeParens(site_name))) +
    stat_smooth(se = FALSE) +
    scale_color_discrete(name="Crop") +
    facet_grid(variable ~ buff_size, scales = "free") +
    ylab(label = "Lethal Doses (from LD50)                         Lbs Active Ingredient") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) + 
    xlab(label = "Year") +
    ggtitle("Yearly Trends in Total Lbs AI & Total Lethal Doses (from LD50) \n by crops with greatest contribution")
```

Chemical Halflife: does it contribute to LD50 presence?
--------------------------------------------------------------------------------

The following plot aims to show the residual LD50 presence within 5 mi of almond buffers in each CA county. The data is grouped by county and year and shows LD50 presence on each day of the almond bloom. These Lethal doses, however, do not come from applications during the bloom, but rather from applications that were made within 5 mi of an almond section during the period December (of the previous year) - Feb 1st. Thus, the plot gives a rough estimate of the number of lethal doses present in nearby sections before any other bloom-period applications are made.

```{r, echo = FALSE, warning = FALSE, fig.width = 16, fig.height = 8}
dat <- as.data.frame(dat)
data <- as.data.frame(data)

dat_melt <- melt(dat, id.vars = c("county", "year", "buff_size", "day"))

ggplot(dat_melt %>% filter(buff_size == 5),
       aes(x = day,
           y = value,
           group = county,
           color = county
           )) +
    geom_line() +
