### todo: this this up it looks like crap

## fyi
## the average honey bee weighs 1/10 of a gram i.e. ~.00022 lbs.
## 1 lb = 453.591 g

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

## change codes to characters:
pur_site$site_code <- as.character(pur_site$site_code)
pur_chem$chem_code <- as.character(pur_chem$chem_code)
yearly_chem_by_crop$site_code <- as.character(yearly_chem_by_crop$site_code)

## get some helper functions (env hf) just in case
source("R/helper_functions.R") ## local machine

## load LD50 for pesticides
tox <- read.csv("BeePUR/AI-BeeToxicity.csv") ## local machine
tox$chem_code <- as.character(tox$chem_code)

## load crop attractiveness to bees:
load("BeePUR/cropBeeAttr.rda")

## load ai info:
load("BeePUR/ais_info.rda")

### make a little dictionary relating crops to some
### ranking systems:
crop_lib <-
    site_chem %>%
    dplyr::mutate(site_code = as.numeric(site_code)) %>%
    distinct(site_name, site_code, HB.Nec, HB.Pol) %>%
    select(site_name, site_code, HB.Nec, HB.Pol)

site_freq <- site_freq %>% mutate(site_code = as.numeric(site_code))

## get a rank based on the frequency
ranked_2013 <- left_join(site_freq,
                         crop_lib, by = c("site_code", "site_name")) %>%
    select(year, buff_size, site_code, site_name, perc_freq, HB.Nec, HB.Pol) %>%
    filter(year == 2013 &
           buff_size %in% c(1,5) &
           (HB.Nec %in% c("++", "+") | HB.Pol %in% c("++", "+"))) %>%
    group_by(buff_size) %>%
    top_n(n = 10, wt = perc_freq)

### visualize time series ###
yearly_chem_by_crop <- yearly_chem_by_crop %>%
    dplyr::mutate(site_code = as.numeric(site_code))

tmp <- left_join(yearly_chem_by_crop, crop_lib, by = "site_code")
ranks_by_use <- tmp %>%
    filter(FALSE | HB.Nec %in% c("++", "+") | HB.Pol %in% c("++", "+")) %>%
    group_by(year, buff_size, site_code, site_name) %>%
    dplyr::summarise(mean_total_lbs = mean(total_lbs, na.rm = TRUE),
                     mean_total_lds = mean(total_LDs, na.rm = TRUE),
                     mean_median_lbs = mean(med_lbs_acre_planted, na.rm = TRUE),
                     mean_median_lds = mean(med_LD_acre_planted, na.rm = TRUE)) %>%
    top_n(10, wt = mean_median_lds)
    
## print.data.frame(ranks_by_use %>%
##                  filter(year == 2013 &
##                         buff_size == 5 &

##                  top_n(10, wt = mean_total_lds) %>%
##                  arrange(desc(mean_med_lds)) %>%
##                  select(site_name, HB.Nec, HB.Pol, mean_med_lds)))

## let's look at the most frequent crops for now:
tmp <- left_join(ranked_2013 %>% select(-year),
                 yearly_chem_by_crop %>% filter(aer_gnd_ind == "tot" & year > 1995),
                 by = c("buff_size", "site_code"))

ggplot(tmp,
       aes(x = year, y = med_LD_acre_planted,
           group = site_code,
           color = as.factor(paste(
               hf$trim(hf$removeParens(site_name)),
               "Nec:", HB.Nec,
               "Pol:", HB.Pol,
               sep = " ")))) +
    geom_smooth(span = .5, se = FALSE, size = 2) +
    facet_grid(~buff_size, scale = "free") +
    scale_color_discrete(name = "Crop with Attractiveness rating") +
    xlab("Year") +
    ylab("Median Lethal Doses per acre Planted") +
    ggtitle("Yearly Change in Median Lethal Dose Presence Per Acre \n shown for bee-attractive crops that appear in >= 25% of buffers surrounding bearing almond sections")

                 
## median LD & lbs per acre by buffer at each comtrs
ggplot(yearly_chem_all_crops %>% filter(aer_gnd_ind == "tot"),
       aes(x = year,
           y = med_LD_acre_planted,
           group = as.factor(buff_size),
           color = as.factor(buff_size))) +
    scale_color_discrete(name = "Buffer Radius (Mi)") +
    geom_smooth() +
    scale_color_manual(values = wes_palette("Zissou", 5, type = "continuous")) +
    xlab("Year") +
    ylab("Median LD's per Acre Planted") +
    ggtitle("Yearly Change in Median Lethal Doses Present \n per Acre Planted ")


ggplot(yearly_chem_all_crops %>% filter(aer_gnd_ind == "tot"), aes(x = year,
                                  y = med_lbs_acre_planted,
              group = as.factor(buff_size),
              color = as.factor(buff_size))) +
    scale_color_discrete(name = "Buffer Radius (Mi)") +
    scale_color_manual(values = wes_palette("Zissou", 5, type = "continuous")) +
    geom_smooth(se = FALSE) +
    xlab("Year") +
    ylab("Median lbs per Acre Planted") +
    ggtitle("Yearly Change in Median lbs Present \n per Acre Planted ")

ggplot(yearly_chem_all_crops %>% filter(aer_gnd_ind == "tot"), aes(x = year,
                                  y = total_lbs,
              group = as.factor(buff_size),
              color = as.factor(buff_size))) +
    scale_color_manual(name = "Buffer Radius (Mi)", values = wes_palette("Zissou", 5, type = "continuous")) +
    geom_smooth(se = FALSE) +
    xlab("Year") +
    ylab("Total Lbs") +
    ggtitle("Yealy Trend in Total Lbs AI Present \n Within Bee-Forage Distance of Almond Sections")

ggplot(yearly_chem_all_crops %>%
       filter(aer_gnd_ind == "tot"), aes(x = year,
                  y = total_LDs,
                  group = as.factor(buff_size),
                  color = as.factor(buff_size))) +
    facet_wrap(~buff_size, scale = "free") +
    scale_color_manual(name = "Buffer Radius (Mi)", values = wes_palette("Zissou", 5, type = "continuous")) +
    geom_smooth(se = FALSE) +
    xlab("Year") +
    ylab("Total Lethal Doses") +
    ggtitle("Yearly Trend in Total Lethal Doses Present \n Within Bee-Forage Distance of Almond Sections")


### Use Boxplots instead?

bplot_stats <- ddply(yearly_chem_all_crops %>% filter(aer_gnd_ind == "tot"),
      .(year, buff_size),
      function(d){
          d1 <- data.frame(year = unique(d$year), buff_size = unique(d$buff_size))
          d1$median_lds <- boxplot.stats(d$med_LD_acre_planted)$stats[5]
          d1$total_lbs <- boxplot.stats(d$total_lbs)$stats[5]
          d1$total_lds <- boxplot.stats(d$total_LDs)$stats[5]
          d1$median_lbs <- boxplot.stats(d$med_lbs_acre_planted)$stats[5]
          d1
      })


bplot_stats <- data.table(melt(bplot_stats, id = c("year", "buff_size")))

ggplot(yearly_chem_all_crops %>% filter(aer_gnd_ind == "tot" &
                                        buff_size == 5 &
                                        year > 1990),
       aes(x = year,
           y = total_LDs,
           group = year)) +
    geom_boxplot(outlier.shape = NA) +
    coord_cartesian(ylim = c(0, bplot_stats[buff_size == 5 &
                        variable == "tot_lds", max(value)])) +
    xlab("Year") +
    ylab("Median LD's per Acre Planted") +
    ggtitle("Yearly Change in Median Lethal Doses Present \n per Acre Planted ")



### visualize lbs_acre vs buff_size ###
ggplot(d, aes(x = buff_size,
              y = lbs_acre,
              group = as.factor(year),
              color = as.factor(year))) +
    stat_smooth(method = "lm")
## since the slope is decreasing, we can surmise
## that total lbs of chemical sprayed does not
## grow propotionally to the acreage of the
## increasing buffer size. Thinking about this
## in terms of an individual bee's risk of contact,
## we can say that, probabilistically, the 1 mile
## buffer is where they are most likely to come into
## contact with one of the Active Ingredients.

### quantify the percent of comtrs that are within range of a
### bee-attractive crop

load("~/Documents/ZhangLab/BeePUR/crop_counts.rda")

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
                              name = "Fraction of Almond Sites") +
                                  ylab("Number of \n Bee-Attractive Crops") +
                                      ggtitle(paste0(buff, " Mi Buffer"))
})

grid.arrange(arrangeGrob(g[[2]], g[[3]], g[[4]], ncol = 3), arrangeGrob(g[[1]], g[[5]], ncol = 2))

ggplot(k, aes(x = year, y = perc_bee_crop_usage)) + geom_line()

## and the distribution for 2013
count_proximity_bee_crops$buff_size <- as.factor(count_proximity_bee_crops$buff_size)
levels(count_proximity_bee_crops$buff_size) <- paste0(1:5, " Mi")
ggplot(count_proximity_bee_crops %>% filter(year == 2013), aes(x = num_bee_crops)) +
    geom_bar(aes(y = (..count..)/4427)) +
    facet_grid(~buff_size) +
    ylab("Percent of all Sections") +
    xlab("Number of unique Bee-Attractive Crops Present in Buffer") +
    ggtitle("Distributions of Bee-Attractive Crops by Buffer size in 2013")



### visualize the percentage of use in bee crops:
bee_crop_use_summary <- perc_chem_use_by_site %>%
    group_by(year, buff_size) %>%
    filter(HB.Nec %in% c("++","+") | HB.Pol %in% c("++","+")) %>%
    dplyr::summarise(total_perc = sum(percent_use))

ggplot(bee_crop_use_summary, aes(x = year, y = total_perc,
                                 group = as.factor(buff_size),
                                 color = as.factor(buff_size))) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = wes_palette("Zissou", 5, type = "continuous"),
                       name = "Buffer Size (Mi)") +
    xlab("Year") +
    ylab("Percent") +
    ggtitle("Percent of Active Ingredient use in Bee-Attractive Crops")


################################################################################
## ai_class

ggplot(yearly_sums_ai_class, aes(x = year, y = value, group = ai_class, color = ai_class)) +
    stat_smooth(se = FALSE) +
    facet_grid(variable ~ buff_size, scale = "free")

ggplot(yearly_sums_AGO_ai_class %>% filter(buff_size == 5),
       aes(x = year, y = value, group = ai_class, color = ai_class)) +
    stat_smooth(se = FALSE) +
    facet_grid(variable ~ aer_gnd_ind, scale = "free")





tmp1 <- ais_info %>%
    arrange(desc(as.numeric(aerobic_half_life_in_soil))) %>%
    select(chemname)

ais_info$chemname <- factor(ais_info$chemname, levels = tmp1$chemname)

g1 <- ggplot(ais_info,
             aes(x = chemname, y = as.numeric(aerobic_half_life_in_soil))) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 5)) +
    ylab("Aerobic HL in Soil") +
    facet_grid(~ai_class, scale = "free")

tmp2 <- ais_info %>%
    arrange(desc(as.numeric(anaerobic_half_life_in_soil))) %>%
    select(chemname)

ais_info$chemname <- factor(ais_info$chemname, levels = tmp2$chemname)

g2 <- ggplot(ais_info,
             aes(x = chemname, y = as.numeric(anaerobic_half_life_in_soil))) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 5)) +
    ylab("Anaerobic HL in Soil") +
    facet_grid(~ai_class, scale = "free")

tmp3 <- ais_info %>%
    arrange(desc(as.numeric(half_life_in_water))) %>%
    select(chemname)

ais_info$chemname <- factor(ais_info$chemname, levels = tmp3$chemname)

g3 <- ggplot(ais_info, aes(x = chemname, y = as.numeric(half_life_in_water))) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 5)) +
    ylab("HL in Water") +
    facet_grid(~ai_class, scale = "free")


do.call(grid.arrange, list(g1,g2,g3))
