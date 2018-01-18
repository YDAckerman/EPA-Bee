setwd("~/Dropbox/ZhangLabData")
load("BeePUR/YA_bee_analysis.rda")
load("BeePUR/yearly_sums.rda")
load("BeePUR/site_frequencies.rda")
load("BeePUR/ais_info.rda")
library(dplyr)
library(ggplot2)
library(plyr)
library(reshape)
library(wesanderson)
library(data.table)
library(stringdist)
library(gridExtra)
library(shiny)


## change codes to characters:
pur_site$site_code <- as.character(pur_site$site_code)
pur_chem$chem_code <- as.character(pur_chem$chem_code)
yearly_chem_by_crop$site_code <- as.character(yearly_chem_by_crop$site_code)

## get some helper functions (env hf) just in case
source("~/Documents/Coding/R/R_convenience/helper_functions.R") ## local machine

## load LD50 for pesticides
tox <- read.csv("BeePUR/AI-BeeToxicity.csv") ## local machine
tox$chem_code <- as.character(tox$chem_code)

lb_to_kg <- 0.453592

## load crop attractiveness to bees:
load("BeePUR/cropBeeAttr.rda")

### visualize the percentage of use in bee crops:
bee_crop_use_summary <- perc_chem_use_by_site %>%
    group_by(year, buff_size) %>%
    filter(HB.Nec %in% c("++","+") | HB.Pol %in% c("++","+")) %>%
    dplyr::summarise(total_perc = sum(percent_use))

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

##g <- arrangeGrob(arrangeGrob(g1, ncol = 1), arrangeGrob(g2, ncol = 1))

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

bins <- c(list(lb_to_kg * c(0.0001, 100)),
          list(lb_to_kg * c(101,200)),
	  list(lb_to_kg * c(201,2000)),
	  list(lb_to_kg * c(2001,20000)),
          list(lb_to_kg * c(20001,60500)))

site_chem_2013$binned_applications <-
    unlist(llply(lb_to_kg * site_chem_2013$total_lbs,
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

## all chemical classes
ggplot(site_chem_2013, aes(x = site_name,
                           y = chemname_w_LD50,
                           fill = binned_applications)) +
    geom_tile() +
    scale_fill_manual(name = "Total Kgs \n Applied",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels,
                      drop = FALSE) +
    labs(title = "Total Kgs Active Ingredient Presence by Crop and AI \n for 5 mi buffered, Bee-Attractive Crops in 2013",
           x = "Crop \n (ordered by decreasing rank of LD contribution)",
	   y = "Active Ingredient & LD50 \n (ordered by increasing LD50)") +
    theme(plot.title = element_text(size = 16),
          axis.title.x = element_text(size = 11),
	  axis.title.y = element_text(size = 11),
          axis.text.x = element_text(angle = 90, hjust = 0, size = 5),
	  axis.text.y = element_text(angle = 0, hjust = 0, size = 4)##,
	  ##legend.position = c(-.08,-.48),
	  ##legend.key.size = unit(.5, "cm")
	  )

## neonics
ggplot(site_chem_2013 %>% filter(ai_class == "NEONICOTINOID"), aes(x = site_name,
                           y = chemname_w_LD50,
                           fill = binned_applications)) +
    geom_tile() +
    scale_fill_manual(name = "Total Kgs \n Applied",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels,
                      drop = FALSE) +
    labs(title = "Total Kgs Active Ingredient Presence by Crop and AI \n for 5 mi buffered, Bee-Attractive Crops in 2013",
           x = "Crop \n (ordered by decreasing rank of LD contribution)",
	   y = "Active Ingredient & LD50 \n (ordered by increasing LD50)") +
    theme(plot.title = element_text(size = 16),
          axis.title.x = element_text(size = 11),
	  axis.title.y = element_text(size = 11),
          axis.text.x = element_text(angle = 90, hjust = 0, size = 5),
	  axis.text.y = element_text(angle = 0, hjust = 0, size = 4)##,
	  ##legend.position = c(-.08,-.48),
	  ##legend.key.size = unit(.5, "cm")
	  )

## Organophosphate
ggplot(site_chem_2013 %>% filter(ai_class == "ORGANOPHOSPHATE"), aes(x = site_name,
                           y = chemname_w_LD50,
                           fill = binned_applications)) +
    geom_tile() +
    scale_fill_manual(name = "Total Kgs \n Applied",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels,
                      drop = FALSE) +
    labs(title = "Total Kgs Active Ingredient Presence by Crop and AI \n for 5 mi buffered, Bee-Attractive Crops in 2013",
           x = "Crop \n (ordered by decreasing rank of LD contribution)",
	   y = "Active Ingredient & LD50 \n (ordered by increasing LD50)") +
    theme(plot.title = element_text(size = 16),
          axis.title.x = element_text(size = 11),
	  axis.title.y = element_text(size = 11),
          axis.text.x = element_text(angle = 90, hjust = 0, size = 5),
	  axis.text.y = element_text(angle = 0, hjust = 0, size = 4)##,
	  ##legend.position = c(-.08,-.48),
	  ##legend.key.size = unit(.5, "cm")
	  )

## Pyrethroid
ggplot(site_chem_2013 %>% filter(ai_class == "PYRETHROID"), aes(x = site_name,
                           y = chemname_w_LD50,
                           fill = binned_applications)) +
    geom_tile() +
    scale_fill_manual(name = "Total Kgs \n Applied",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels,
                      drop = FALSE) +
    labs(title = "Total Kgs Active Ingredient Presence by Crop and AI \n for 5 mi buffered, Bee-Attractive Crops in 2013",
           x = "Crop \n (ordered by decreasing rank of LD contribution)",
	   y = "Active Ingredient & LD50 \n (ordered by increasing LD50)") +
    theme(plot.title = element_text(size = 16),
          axis.title.x = element_text(size = 11),
	  axis.title.y = element_text(size = 11),
          axis.text.x = element_text(angle = 90, hjust = 0, size = 5),
	  axis.text.y = element_text(angle = 0, hjust = 0, size = 4)##,
	  ##legend.position = c(-.08,-.48),
	  ##legend.key.size = unit(.5, "cm")
	  )

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

## all classes
ggplot(site_chem_2013, aes(x = site_name,
                           y = chemname_w_LD50,
                           fill = binned_applications)) +
    geom_tile() +
    labs(title = "Total Lethal Doses Presence by Crop and AI \n for 5 mi buffered, Bee-Attractive Crops in 2013",
           x = "Crop \n (ordered by decreasing rank of LD contribution)",
	   y = "Active Ingredient & LD50 \n (ordered by increasing LD50)") +
    theme(plot.title = element_text(size = 16),
          axis.text.x = element_text(angle = 90, hjust = 0, size = 5),
	  axis.text.y = element_text(angle = 0, hjust = 0, size = 4)##,
	  ##legend.position = c(-.08,-.38),
	  ##legend.key.size = unit(.3, "cm")
	  ) +
    scale_fill_manual(name = "Total \n Lethal Doses",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels,
                      drop = FALSE)

## neonics
ggplot(site_chem_2013 %>% filter(ai_class == "NEONICOTINOID"), aes(x = site_name,
                           y = chemname_w_LD50,
                           fill = binned_applications)) +
    geom_tile() +
    labs(title = "Total Lethal Doses Presence by Crop and AI \n for 5 mi buffered, Bee-Attractive Crops in 2013",
           x = "Crop \n (ordered by decreasing rank of LD contribution)",
	   y = "Active Ingredient & LD50 \n (ordered by increasing LD50)") +
    theme(plot.title = element_text(size = 16),
          axis.text.x = element_text(angle = 90, hjust = 0, size = 5),
	  axis.text.y = element_text(angle = 0, hjust = 0, size = 4)##,
	  ##legend.position = c(-.08,-.38),
	  ##legend.key.size = unit(.3, "cm")
	  ) +
    scale_fill_manual(name = "Total \n Lethal Doses",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels,
                      drop = FALSE)

## organophosphate
ggplot(site_chem_2013 %>% filter(ai_class == "ORGANOPHOSPHATE"), aes(x = site_name,
                           y = chemname_w_LD50,
                           fill = binned_applications)) +
    geom_tile() +
    labs(title = "Total Lethal Doses Presence by Crop and AI \n for 5 mi buffered, Bee-Attractive Crops in 2013",
           x = "Crop \n (ordered by decreasing rank of LD contribution)",
	   y = "Active Ingredient & LD50 \n (ordered by increasing LD50)") +
    theme(plot.title = element_text(size = 16),
          axis.text.x = element_text(angle = 90, hjust = 0, size = 5),
	  axis.text.y = element_text(angle = 0, hjust = 0, size = 4)##,
	  ##legend.position = c(-.08,-.38),
	  ##legend.key.size = unit(.3, "cm")
	  ) +
    scale_fill_manual(name = "Total \n Lethal Doses",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels,
                      drop = FALSE)

## pyrethroid
ggplot(site_chem_2013 %>% filter(ai_class == "PYRETHROID"), aes(x = site_name,
                           y = chemname_w_LD50,
                           fill = binned_applications)) +
    geom_tile() +
    labs(title = "Total Lethal Doses Presence by Crop and AI \n for 5 mi buffered, Bee-Attractive Crops in 2013",
           x = "Crop \n (ordered by decreasing rank of LD contribution)",
	   y = "Active Ingredient & LD50 \n (ordered by increasing LD50)") +
    theme(plot.title = element_text(size = 16),
          axis.text.x = element_text(angle = 90, hjust = 0, size = 5),
	  axis.text.y = element_text(angle = 0, hjust = 0, size = 4)##,
	  ##legend.position = c(-.08,-.38),
	  ##legend.key.size = unit(.3, "cm")
	  ) +
    scale_fill_manual(name = "Total \n Lethal Doses",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels,
                      drop = FALSE)


bins <- c(list(0), list(1:5), list(6:10), list(11:20), list(21:29))
          
det_bins <- function(.d){
    ldply(bins, function(bin){
        tmp <- .d %>%
             filter( (num_bee_crops <= max(bin) & num_bee_crops >= min(bin)))
        data.frame(bin = paste0(min(bin), "-", max(bin)),
                   num_comtrs = length(unique(tmp$comtrs)),
                   tot_comtrs = length(unique(.d$comtrs)),
                   stringsAsFactors = FALSE)
    })
}
d1 <- count_proximity_bee_crops %>%
    group_by(year, buff_size) %>%
    do(det_bins(.)) %>%
    mutate(perc_comtrs = num_comtrs / tot_comtrs)

g <- llply(1:5, function(buff){
    d11 <- d1 %>% filter(buff_size == buff)
    d11$bin <- factor(d11$bin, levels = unique(d11$bin))
    d11$rnd_perc <- as.factor(round(d11$perc_comtrs,1))
    ggplot(d11, aes( x = year, y = bin)) +
        geom_tile(aes(fill = rnd_perc)) +
            scale_fill_manual(values = wes_palette("Zissou", length(unique(d11$rnd_perc)), "continuous"),
                              name = "Proportion of \n Almond Sites") +
                                  ylab("Number of \n Bee-Attractive Crops") +
                                      ggtitle(paste0(buff, " Mi Buffer"))
})

grid.arrange(arrangeGrob(g[[2]], g[[3]], g[[4]], ncol = 3), arrangeGrob(g[[1]], g[[5]], ncol = 2))
g <- arrangeGrob(arrangeGrob(g[[2]], g[[3]], g[[4]], ncol = 3), arrangeGrob(g[[1]], g[[5]], ncol = 2))

## histogram of distribution of number of bee-attractive crops within 1-5 mile buffer
## of bearing almond sections
count_proximity_bee_crops$buff_size <- as.factor(count_proximity_bee_crops$buff_size)
levels(count_proximity_bee_crops$buff_size) <- paste0(1:5, " Mi")
ggplot(count_proximity_bee_crops %>% filter(year == 2013), aes(x = num_bee_crops)) +
    geom_bar(aes(y = (..count..)/4427)) +
    facet_grid(~buff_size) +
    ylab("Proportion of all Almond Sections") +
    xlab("Number of unique Bee-Attractive Crops Present in Buffer") +
    ggtitle("Distributions of Number of Unique, Non-Almond, Bee-Attractive Crops \n by Buffer size in 2013")

## histogram of distribution of number of chemicals used (on bee crop) within 1-5 mile buffer
## of bearing almond sections
dd <- count_proximity_bee_crops %>%
    filter(year == 2013) %>%
    group_by(buff_size, num_chems_used) %>%
    dplyr::summarise(proportion = n() / 4427) %>%
    ungroup()

ggplot(dd, aes(x = num_chems_used, y = proportion)) +
    geom_bar(stat = "identity") +
    facet_grid(~buff_size) +
    ylab("Proportion of all Almond Sections") +
    xlab("Number of unique Chemicals Used in Buffer") +
    ggtitle("Distributions of Number of Unique Chemicals Used \n by Buffer size in 2013")

## histogram of distribution of number of (all) crops within 1-5 mile buffer
## of bearing almond sections
count_proximity_all_crops$buff_size <- as.factor(count_proximity_all_crops$buff_size)
levels(count_proximity_all_crops$buff_size) <- paste0(1:5, " Mi")
ggplot(count_proximity_all_crops %>% filter(year == 2013), aes(x = num_bee_crops)) +
    geom_bar(aes(y = (..count..)/4427)) +
    facet_grid(~buff_size) +
    ylab("Proportion of all Almond Sections") +
    xlab("Number of unique Crops Present in Buffer") +
    ggtitle("Distributions of Number of Unique, Non-Almond, All-Attractive Crops \n by Buffer size in 2013")


## histogram of distribution of number of chemicals used (on all crop) within 1-5 mile buffer
## of bearing almond sections
dd <- count_proximity_all_crops %>%
    filter(year == 2013) %>%
    group_by(buff_size, num_chems_used) %>%
    dplyr::summarise(proportion = n() / 4427) %>%
    ungroup()

ggplot(dd, aes(x = num_chems_used, y = proportion)) +
    geom_bar(stat = "identity") +
    facet_grid(~buff_size) +
    ylab("Proportion of all Almond Sections") +
    xlab("Number of unique Chemicals Used in Buffer") +
    ggtitle("Distributions of Number of Unique Chemicals Used \n by Buffer size in 2013")

yearly_sums$buff_size <- as.factor(yearly_sums$buff_size)
levels(yearly_sums$buff_size) <- paste0(1:5, " Mi")
yearly_sums$variable <- gsub("lbs", "kgs", yearly_sums$variable)
yearly_sums$variable <- as.factor(yearly_sums$variable)
yearly_sums <- yearly_sums %>%
    dplyr::mutate(value = ifelse(grepl("kgs", variable),
                                 lb_to_kg * value, value))
levels(yearly_sums$variable) <- c("Total kgs AI", "Total Lethal Doses", "Median kgs", "Median lds", "Mean kgs", "Mean lds")

ggplot(yearly_sums %>% filter(variable %in% c("Total kgs AI", "Total Lethal Doses")),
       aes(x = year, y = value,
           group = variable, color = variable)) +
    scale_color_manual(values = wes_palette("Cavalcanti")) +
    geom_line() +
    ylab(label = "Lethal Doses (from LD50)                              Kgs Active Ingredient") +
    facet_grid(variable ~ buff_size, scale = "free") +
    xlab(label = "Year") +
    ggtitle("Yearly Trends in Total Kgs AI & Total Lethal Doses (from LD50)")

## by ai_class

ggplot(yearly_sums_ai_class, aes(x = year, y = value, group = ai_class, color = ai_class)) +
    stat_smooth(se = FALSE) +
    facet_grid(variable ~ buff_size, scale = "free")

ggplot(yearly_sums_AGO_ai_class %>% filter(buff_size == 5),
       aes(x = year, y = value, group = ai_class, color = ai_class)) +
    stat_smooth(se = FALSE) +
    facet_grid(variable ~ aer_gnd_ind, scale = "free")

## look at Half Life

tmp1 <- ais_info %>%
    arrange(desc(as.numeric(aerobic_half_life_in_soil))) %>%
    select(chemname)

ais_info$chemname <- factor(ais_info$chemname, levels = tmp1$chemname)

g1 <- ggplot(ais_info,
             aes(x = chemname, y = as.numeric(aerobic_half_life_in_soil))) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 5)) +
    ylab("Aerobic HL in Soil (days)") +
    facet_grid(~ai_class, scale = "free")

tmp2 <- ais_info %>%
    arrange(desc(as.numeric(anaerobic_half_life_in_soil))) %>%
    select(chemname)

ais_info$chemname <- factor(ais_info$chemname, levels = tmp2$chemname)

g2 <- ggplot(ais_info,
             aes(x = chemname, y = as.numeric(anaerobic_half_life_in_soil))) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 5)) +
    ylab("Anaerobic HL in Soil (days)") +
    facet_grid(~ai_class, scale = "free")

tmp3 <- ais_info %>%
    arrange(desc(as.numeric(half_life_in_water))) %>%
    select(chemname)

ais_info$chemname <- factor(ais_info$chemname, levels = tmp3$chemname)

g3 <- ggplot(ais_info, aes(x = chemname, y = as.numeric(half_life_in_water))) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 5)) +
    ylab("HL in Water (days)") +
    facet_grid(~ai_class, scale = "free")


do.call(grid.arrange, list(g1,g2,g3))

## filter by duration
tmp <- ais_info %>%
    mutate(aerobic_half_life_in_soil = as.numeric(aerobic_half_life_in_soil),
           anaerobic_half_life_in_soil = as.numeric(anaerobic_half_life_in_soil),
           half_life_in_water = as.numeric(half_life_in_water)) %>%
    filter(aerobic_half_life_in_soil >= 60 | anaerobic_half_life_in_soil >= 60 | half_life_in_water >= 60) %>%
    mutate(chem_code = as.character(chem_code)) %>%
    select(chem_code)


ggplot(site_chem_2013 %>% filter(chem_code %in% tmp$chem_code), aes(x = site_name,
                           y = chemname_w_LD50,
                           fill = as.factor(binned_applications))) +
    geom_tile() +
    labs(title = "Total Lethal Doses Presence by Crop and AI \n for 5 mi buffered, Bee-Attractive Crops in 2013",
           x = "Crop \n (ordered by decreasing rank of LD contribution)",
	   y = "Active Ingredient & LD50 \n (ordered by increasing LD50)") +
    theme(plot.title = element_text(size = 16),
          axis.text.x = element_text(angle = 90, hjust = 0, size = 5),
	  axis.text.y = element_text(angle = 0, hjust = 0, size = 4)##,
	  ##legend.position = c(-.08,-.38),
	  ##legend.key.size = unit(.3, "cm")
	  ) +
    scale_fill_manual(name = "Total \n Lethal Doses",
                      values = pal,
                      breaks = seq_along(labels),
                      labels = labels)

## ## ##

top_ten <- order1 %>% head(10)

yearly_sums_by_site$buff_size <- as.factor(yearly_sums_by_site$buff_size)
levels(yearly_sums_by_site$buff_size) <- paste0(1:5, " Mi")
yearly_sums_by_site$variable <- gsub("lbs", "kgs",
                                     yearly_sums_by_site$variable)
yearly_sums_by_site <- yearly_sums_by_site %>%
    dplyr::mutate(value = ifelse(grepl("kgs", variable),
                                 lb_to_kg * value, value))
yearly_sums_by_site$variable <- as.factor(yearly_sums_by_site$variable)
levels(yearly_sums_by_site$variable) <- c("Total kgs AI", "Total Lethal Doses", "Median kgs", "Median lds", "Mean kgs", "Mean lds")

ggplot(yearly_sums_by_site %>%
       filter(site_code %in% as.numeric(top_ten$site_code) &
       variable %in% c("Total kgs AI", "Total Lethal Doses")),
       aes(x = year, y = value, group = site_name, color = hf$removeParens(site_name))) +
    geom_line() +
    scale_color_discrete(name="Crop") +
    facet_grid(variable ~ buff_size, scales = "free") +
    ylab(label = "Lethal Doses (from LD50)                         Kgs Active Ingredient") +
    xlab(label = "Year") +
    ggtitle("Yearly Trends in Total Kgs AI & Total Lethal Doses (from LD50) \n by crops with greatest contribution")
