#### ON abound ####

### load libraries

library(RPostgreSQL)
library(DBI)
library(dplyr)
library(ggplot2)
library(plyr)
library(stringdist)

### load data, functions, etc.

## get hf (helper functions)
source("~/R/helper_functions.R")


con <- src_postgres(dbname="california",
                  host="ziram.lawr.ucdavis.edu",
                   user="jonathan"
                   ## ,options="-c search_path=bee"
                   )

## connect to the various bee data sets
bee_buf <- tbl(con, dplyr::sql("SELECT * FROM bee.almond_buff_chem_use_4_sum"))
bee_buf_bearing <- collect(tbl(con, dplyr::sql("SELECT * FROM bee.almond_sections_bearing")))
#prod_section <- tbl(con, dplyr::sql("SELECT * FROM bee.prod_use_records"))
prod_section <- tbl(con, dplyr::sql("SELECT * FROM bee.almond_buff_prod_use_bearing"))
bee_buf2 <- tbl(con, dplyr::sql("SELECT * FROM bee.almond_buff_chem_use_bearing_sum"))


## get ai info
ai_use_type <- tbl(con, dplyr::sql("SELECT * FROM pur.dpr_ai_categories"))
ai_use_type <- collect(ai_use_type)

## get crop info
pur_site <- tbl(con, dplyr::sql("SELECT * FROM pur.dpr_site"))
pur_site <- collect(pur_site)

## get chem info
pur_chem <- tbl(con, dplyr::sql("SELECT * FROM pur.chemical"))
pur_chem <- collect(pur_chem)
## get product info
pur_prod <- tbl(con, dplyr::sql("SELECT * FROM pur.product"))
pur_prod <- collect(pur_prod)

## get toxicity info
tox <- read.csv("~/Bee_Data/AI-BeeToxicity.csv", stringsAsFactors = FALSE)

## get crop attractiveness info
cropAttr <- read.csv("~/Bee_Data/cropBeeAttr.csv", stringsAsFactors = FALSE)
cropAttr$Crop <- hf$trim(cropAttr$Crop)

## change codes from numbers to characters
##pur_site$site_code <- as.character(pur_site$site_code)
##pur_chem$chem_code <- as.character(pur_chem$chem_code)
##tox$chem_code <- as.character(tox$chem_code)

##########################################################################
##########################################################################
## site_chem

## goal: we want three id columns: site_code, buff_size, and chem_code
## plus X columns that communicate the relationship between chem_code
## and site_code at the level of buff_size. The assumption we made in
## the above summary - i.e. that #lbs / total acre of buffer is a reasonable
## metric of bee risk - will still have to do here. This time, however, we'll
## see how much of each site_code/chem_code combination contributes to the
## overall total represented in d.

## filter out the 0 acre planted rows (these aren't crops?)

lb_to_kg <- 0.453592
ac_to_hc <- 0.404686

## air, ground, and other:
site_chem_AGO <- bee_buf2 %>%
    ## remove non-crops
    ##dplyr::filter(acre_planted != 0) %>%
    ## create lbs per acre planted variable
    ##dplyr::mutate(lbs_acre_planted = lbs_chem / acre_planted) %>%
    ## group
    group_by(year, buff_size, site_code, chem_code, aer_gnd_ind) %>%
    ## summarize into total lbs, av lbs per acre, and variance of
    ## lbs per acre
    dplyr::summarise(num_applications = n(),
                     total_lbs = sum(lbs_chem),
		     total_kg = lb_to_kg * sum(lbs_chem) ##,
                     ##av_lbs_acre_planted = mean(lbs_acre_planted),
                     ##var_lbs_acre_planted  = var(lbs_acre_planted)
                     ) %>% ungroup()
## do the same as above, but do not group by aer_gnd_ind (so this
## is a summary for all usage types)
site_chem_tot <- bee_buf2 %>%
    ##dplyr::filter(acre_planted != 0) %>%
    ##dplyr::mutate(lbs_acre_planted = lbs_chem / acre_planted,
        kgs_acre_planted = lb_to_kg * lbs_chem / acre_planted
    ) %>%
    group_by(year, buff_size, site_code, chem_code) %>%
    dplyr::summarise(num_applications = n(),
                     total_lbs = sum(lbs_chem),
		     total_kg = lb_to_kg * sum(lbs_chem)##,
                     ##av_lbs_acre_planted = mean(lbs_acre_planted),
                     ##var_lbs_acre_planted  = var(lbs_acre_planted)
                     ) %>% ungroup()

## collect
site_chem_AGO <- collect(site_chem_AGO)
site_chem_tot <- collect(site_chem_tot)
## create "tot" value in aer_gnd_ind so we can bind
site_chem_tot$aer_gnd_ind <- "tot"
## bind
site_chem <- rbind(site_chem_AGO, site_chem_tot)
## convert codes to characters

## since median isn't avaible on dplyr - sql, collect by year
## and summarize locally to find it
## site_chem_med <- ldply(1991:2013, function(Y){
##     ## filter by the year
##     site_chem <- bee_buf2 %>%
##         filter(year == Y)
##     ## collect the now reasonable sized db
##     site_chem <- collect(site_chem)
##     ## convert codes to chars
##     site_chem$chem_code <- as.character(site_chem$chem_code)
##     site_chem$site_code <- as.character(site_chem$site_code)
##     ## order_freq <- site_chem TODO: figure out a good order
##     site_chem %>%
##         ## filter out non-crops
##         ## filter(acre_planted != 0) %>%
##         ## create lbs_per_acre
##         dplyr::mutate(##lbs_per_acre = lbs_chem / acre_planted,
##                       aer_gnd_ind = "tot") %>%
##         group_by(year, buff_size, site_code, chem_code, aer_gnd_ind) %>%
##         ## create new variable for the median of lbs_per_acre                      
##         dplyr::summarise(med_lbs_acre_planted = median(lbs_per_acre,
##                              na.rm = TRUE)) %>%
##                                  ungroup()
## }, .progress = "text")

## left join median usage values into site_chem
## site_chem <- left_join(site_chem, site_chem_med, by = c("year", "buff_size",
##                                                        "site_code", "chem_code",
##                                                        "aer_gnd_ind"))

## left  join tox data into site_chem
site_chem <- left_join(site_chem, tox, by = "chem_code")
## mutate lbs per acre measurements into new lethal dose per acre
## measurements
site_chem <- site_chem %>%
    dplyr::mutate(##av_LD_acre_planted = 
                  ##453.592 * av_lbs_acre_planted / (LD50..ug.bee.1. / (10^6)),
                  ##med_LD_acre_planted = 
                  ##453.592 * med_lbs_acre_planted / (LD50..ug.bee.1. / (10^6)),
                  total_lds = 453.592 * total_lbs / (LD50..ug.bee.1. / (10^6))
                  ) 

## add product and crop names to site_chem
site_chem <- left_join(site_chem, pur_site, by = "site_code")
site_chem <- left_join(site_chem, pur_chem, by = "chem_code")

## add crop attractiveness to site_chem
crop_attr_bees <- ldply(unique(site_chem$site_name), function(crop1){
    crop <- hf$trim(unlist(llply(
        strsplit(unique(hf$removeParens(crop1)), ","),
        function(x) x[[1]])))
    ## spot corrections
    crop <- switch(crop,
                   'BEETS' = "BEET",
                   'SUGARBEET' = "BEET",
                   'OATS' = "OAT",
                   'TOMATOES' = "TOMATO",
                   'SORGHUM/MILO  GENERAL' = "SORGHUM",
                   'RAPE' = "RAPESEED",
                   "RYEGRASS" = "RYE GRASS FOR FORAGE AND SILAGE",
                   crop
                   )
    v <- try(cropAttr %>% filter(grepl(crop, Crop, ignore.case = TRUE)))
    if(class(v) == "try-error"){
        print(crop)
        return(NULL)
    }
    if(nrow(v) == 0 | is.na(crop)){
        v <- as.data.frame(setNames(as.list(rep(NA, ncol(v))),
                                    colnames(v)))
        data.frame(C = crop1, v,
                   stringsAsFactors = FALSE)
    } else {
        v <- v %>%
            mutate(dist = stringdist(crop, Crop)) %>%
                slice(which.min(dist))
        data.frame(C = crop1, v,
                   stringsAsFactors = FALSE)
    }
})

site_chem <- left_join(site_chem,
                       crop_attr_bees %>%
                       select(C, HB.Pol, HB.Nec),
                  by = c("site_name" = "C"))

## now that we know what crops are most attractive, we'll be able to
## focus our time series analysis. to do so, it will be useful to have
## a decent relation between the pur site stuff and the info brought in
## from elsewhere:

crop_lib <- site_chem %>%
    distinct(site_name, site_code, HB.Nec, HB.Pol) %>%
    select(site_name, site_code, HB.Nec, HB.Pol)

crop_lib_attr <- crop_lib %>%
    filter(HB.Nec %in% c("++", "+") | HB.Pol %in% c("++", "+"))


###############################################################
###############################################################
## site_prod

site_prod_AGO <- prod_section %>%
    ## remove non-crops
    ##dplyr::filter(acre_planted != 0) %>%
    ## create lbs per acre planted variable
    ##dplyr::mutate(lbs_acre_planted = lbs_prd_used / acre_planted) %>%
    ## group
    group_by(year, buff_size, site_code, prodno, aer_gnd_ind) %>%
    ## summarize into total lbs, av lbs per acre, and variance of
    ## lbs per acre
    dplyr::summarise(total_lbs = sum(lbs_prod)##,
                     ##av_lbs_acre_planted = mean(lbs_acre_planted),
                     ##var_lbs_acre_planted  = var(lbs_acre_planted)
                     ) %>% ungroup()

## do the same as above, but do not group by aer_gnd_ind (so this
## is a summary for all usage types)
site_prod_tot <- prod_section %>%
    ##dplyr::filter(acre_planted != 0) %>%
    ##dplyr::mutate(lbs_acre_planted = lbs_prd_used / acre_planted,
    kgs_acre_planted = lb_to_kgs * lbs_prd_used / acre_planted
          ##        ) %>%
    group_by(year, buff_size, site_code, prodno) %>%
    dplyr::summarise(total_lbs = sum(lbs_prod),
                     total_kgs = lb_to_kg ##,
                     ##av_lbs_acre_planted = mean(lbs_acre_planted),
                     ##var_lbs_acre_planted  = var(lbs_acre_planted)
                     ) %>% ungroup()

## collect
site_prod_AGO <- collect(site_prod_AGO)
site_prod_tot <- collect(site_prod_tot)
site_prod_tot$aer_gnd_ind <- "tot"

## bind
site_prod <- rbind(site_prod_AGO, site_prod_tot)

## since median isn't avaible on dplyr - sql, collect by year
## and summarize to find median of lbs per acre usage
## site_prod_med <- ldply(1991:2013, function(Y){
##     ## filter by the year
##     site_prod <- prod_section %>%
##         filter(year == Y)
##     ## collect the now reasonable sized db
##     site_prod <- collect(site_prod)
##     ## convert codes to chars
##     ## order_freq <- site_prod TODO: figure out a good order
##     site_prod %>%
##         ## filter out non-crops
##         filter(acre_planted != 0) %>%
##         ## create lbs_per_acre
##         dplyr::mutate(lbs_per_acre = lbs_prd_used / acre_planted,
##                       aer_gnd_ind = "tot") %>%
##         group_by(year, site_code, prodno, aer_gnd_ind) %>%
##         ## create new variable for the median of lbs_per_acre                      
##         dplyr::summarise(med_lbs_acre_planted = median(lbs_per_acre,
##                              na.rm = TRUE)) %>%
##                                  ungroup()
## }, .progress = "text")

## left join median usage values into site_prod
## site_prod <- left_join(site_prod, site_prod_med, by = c("year", "site_code", "prodno",
##                                                        "aer_gnd_ind"))

## left join pur_prod info into site_prod
site_prod <- left_join(site_prod, pur_prod, by = "prodno")

##########################################################################
##########################################################################
## yearly summaries

### summarize the data set by year, comtrs, and buff size, using
### total lbs / acre (of buffer) and num air/grnd applications as
### statistics


## todo: incorporate ai categories:
tox$chem_code <- as.numeric(tox$chem_code)
yearly_chem_all_crops <- ldply(c(1991:2013), function(Y){
    comtrs <- bee_buf_bearing %>%
                 filter(year == Y) %>%
		 select(comtrs) %>%
		 distinct(comtrs)
    bee_tox <- bee_buf %>% filter(year == Y)
    bee_tox <- collect(bee_tox)
    bee_tox <- bee_tox %>% filter(acre_planted != 0  & comtrs %in% comtrs$comtrs)
    bee_tox <- left_join(bee_tox, tox, by = "chem_code")
    yearly_chem_all_crops_AOG <- bee_tox %>%
        dplyr::mutate(lbs_acre_planted = lbs_chem / acre_planted,
	              kgs_acre_planted = lb_to_kg * lbs_chem / acre_planted,
                      LD_acre_planted = 453.592 * lbs_chem /
                      (acre_planted * (LD50..ug.bee.1. / (10^6))),
                      LD_pres = 453.592 * lbs_chem / (LD50..ug.bee.1. / (10^6))) %>%
                          group_by(year, comtrs, buff_size, aer_gnd_ind) %>%
                              dplyr::summarise(
                                  total_lbs = sum(lbs_chem),
				  total_kgs = lb_to_kg * sum(lbs_chem),
                                  total_LDs = sum(LD_pres),
                                  av_lbs_acre_planted = mean(lbs_acre_planted),
				  av_kgs_acre_planted = lb_to_kg * mean(lbs_acre_planted),
                                  med_lbs_acre_planted = median(lbs_acre_planted),
				  med_kgs_acre_planted = lb_to_kg * median(lbs_acre_planted),
                                  av_LD_acre_planted = mean(LD_acre_planted),
                                  med_LD_acre_planted = median(LD_acre_planted),
                                  var_lbs_acre_planted = var(lbs_acre_planted),
				  var_kgs_acre_planted = (lb_to_kg)^2 * var(lbs_acre_planted),
                                  var_LD_acre_planted = var(LD_acre_planted)
                                  )
    yearly_chem_all_crops_tot <- bee_tox %>%
        dplyr::mutate(lbs_acre_planted = lbs_chem / acre_planted,
	              kgs_acre_planted = lb_to_kg * lbs_chem / acre_planted,
                      LD_acre_planted = 453.592 * lbs_chem /
                      (acre_planted * (LD50..ug.bee.1. / (10^6))),
                      aer_gnd_ind = "tot",
                      LD_pres = 453.592 * lbs_chem / (LD50..ug.bee.1. / (10^6))) %>%
                          group_by(year, comtrs, buff_size, aer_gnd_ind) %>%
                              dplyr::summarise(
                                  total_lbs = sum(lbs_chem),
				  total_kgs = lb_to_kg * sum(lbs_chem),
                                  total_LDs = sum(LD_pres),
                                  av_lbs_acre_planted = mean(lbs_acre_planted),
				  av_kgs_acre_planted = lb_to_kg * mean(lbs_acre_planted),
                                  med_lbs_acre_planted = median(lbs_acre_planted),
				  med_kgs_acre_planted = lb_to_kg * median(lbs_acre_planted),
                                  av_LD_acre_planted = mean(LD_acre_planted),
                                  med_LD_acre_planted = median(LD_acre_planted),
                                  var_lbs_acre_planted = var(lbs_acre_planted),
				  var_kgs_acre_planted = (lb_to_kg)^2 * var(lbs_acre_planted),
                                  var_LD_acre_planted = var(LD_acre_planted)
                                  )
    tmp <- rbind(yearly_chem_all_crops_AOG,
                 yearly_chem_all_crops_tot)
}, .progress = "text")


### do the same, but for each specific bee crop.
### in this case, however, since we're site specific, we can look
### at the intensity of chemical use within each section of planting.
### thus we'll also calculate the lbs_acre_planted and find the
### sum over all instances of that site in each buffer of the comtrs:

yearly_chem_by_crop <- ldply(c(1991:2013), function(Y){
    comtrs <- bee_buf_bearing %>% filter(year == Y) %>% select(comtrs) %>% distinct(comtrs)
    bee_tox <- bee_buf %>% filter(year == Y)
    bee_tox <- collect(bee_tox)
    bee_tox <- bee_tox %>% filter(acre_planted != 0  & comtrs %in% comtrs$comtrs)
    bee_tox <- left_join(bee_tox, tox, by = "chem_code")
    yearly_chem_by_crop_AOG <- bee_tox %>%
        dplyr::mutate(lbs_acre_planted = lbs_chem / acre_planted,
	              kgs_acre_planted = lb_to_kg * lbs_chem / acre_planted,
                      LD_acre_planted = 453.592 * lbs_chem /
                      (acre_planted * (LD50..ug.bee.1. / (10^6))),
                      LD_pres = 453.592 * lbs_chem / (LD50..ug.bee.1. / (10^6))) %>%
                          group_by(year, comtrs, site_code, buff_size, aer_gnd_ind) %>%
                              dplyr::summarise(
                                  total_lbs = sum(lbs_chem),
				  total_kgs = lb_to_kg * sum(lbs_chem),
                                  total_LDs = sum(LD_pres),
                                  av_lbs_acre_planted = mean(lbs_acre_planted),
				  av_kgs_acre_planted = lb_to_kg * mean(lbs_acre_planted),
                                  med_lbs_acre_planted = median(lbs_acre_planted),
				  med_kgs_acre_planted = lb_to_kg * median(lbs_acre_planted),
                                  av_LD_acre_planted = mean(LD_acre_planted),
                                  med_LD_acre_planted = median(LD_acre_planted),
                                  var_lbs_acre_planted = var(lbs_acre_planted),
				  var_kgs_acre_planted = (lb_to_kg)^2 * var(lbs_acre_planted),
                                  var_LD_acre_planted = var(LD_acre_planted)
                                  )
    yearly_chem_by_crop_tot <- bee_tox %>%
        dplyr::mutate(lbs_acre_planted = lbs_chem / acre_planted,
	              kgs_acre_planted = lb_to_kg * lbs_chem / acre_planted,
                      LD_acre_planted = 453.592 * lbs_chem /
                      (acre_planted * (LD50..ug.bee.1. / (10^6))),
                      aer_gnd_ind = "tot",
                      LD_pres = 453.592 * lbs_chem / (LD50..ug.bee.1. / (10^6))) %>%
                          group_by(year, comtrs, site_code, buff_size, aer_gnd_ind) %>%
                              dplyr::summarise(
                                  total_lbs = sum(lbs_chem),
				  total_kgs = lb_to_kg * sum(lbs_chem),
                                  total_LDs = sum(LD_pres),
                                  av_lbs_acre_planted = mean(lbs_acre_planted),
				  av_kgs_acre_planted = lb_to_kg * mean(lbs_acre_planted),
                                  med_lbs_acre_planted = median(lbs_acre_planted),
				  med_kgs_acre_planted = lb_to_kg * median(lbs_acre_planted),
                                  av_LD_acre_planted = mean(LD_acre_planted),
                                  med_LD_acre_planted = median(LD_acre_planted),
                                  var_lbs_acre_planted = var(lbs_acre_planted),
				  var_kgs_acre_planted = (lb_to_kg)^2 * var(lbs_acre_planted),
                                  var_LD_acre_planted = var(LD_acre_planted)
                                  )
     rbind(yearly_chem_by_crop_AOG,
           yearly_chem_by_crop_tot)
}, .progress = "text")

##########################################################################
##########################################################################
## rank and percentage statistics:

## get a gauge of how many bee crops are in each buffer for each comtrs
## in each year
count_proximity_bee_crops <- ldply(1991:2013, function(Y){

    comtrs <- bee_buf_bearing %>% filter(year == Y) %>% select(comtrs)

    d <- bee_buf %>%
        filter(year == Y)
    d <- collect(d)
    d %>%
        ## filter out almonds because we know almonds are present at every almond section
        filter(site_code %in% setdiff(as.numeric(crop_lib_attr$site_code), 3001) &
               comtrs %in% comtrs$comtrs) %>%
            group_by(year, comtrs, buff_size) %>%
                dplyr::select(site_code, chem_code) %>%
                    dplyr::summarise(num_bee_crops = n_distinct(site_code),
                                     num_chems_used = n_distinct(chem_code))
}, .progress = "text")

count_proximity_all_crops <- ldply(1991:2013, function(Y){

    comtrs <- bee_buf_bearing %>% filter(year == Y) %>% select(comtrs)

    d <- bee_buf %>%
        filter(year == Y)
    d <- collect(d)
    d %>%
        ## filter out almonds because we know almonds are present at every almond section
        filter(site_code != 3001 & comtrs %in% comtrs$comtrs) %>%
            group_by(year, comtrs, buff_size) %>%
                dplyr::select(site_code, chem_code) %>%
                    dplyr::summarise(num_bee_crops = n_distinct(site_code),
                                     num_chems_used = n_distinct(chem_code))
}, .progress = "text")

### rankings:

AI_ranks_all_chem <- bee_buf2 %>%
    group_by(year, buff_size, chem_code) %>%
    dplyr::summarise(total_lbs = sum(lbs_chem),
    total_kgs = lb_to_kg * sum(lbs_chem))
AI_ranks_all_chem <- collect(AI_ranks_all_chem)
AI_ranks_all_chem <- AI_ranks_all_chem %>%
    group_by(year, buff_size, chem_code) %>%
    arrange(desc(total_lbs))
AI_ranks_all_chem$chem_code <- as.character(AI_ranks_all_chem$chem_code)
AI_ranks_all_chem <- left_join(AI_ranks_all_chem, pur_chem,
                               by = "chem_code")
total_lbs <- AI_ranks_all_chem %>%
    group_by(year, buff_size) %>%
    dplyr::summarise(total_lbs_all_chem = sum(total_lbs),
                     total_kgs_all_chem = lb_to_kg * sum(total_lbs))
AI_ranks_all_chem <- left_join(AI_ranks_all_chem, total_lbs,
                               by = c("year", "buff_size"))
AI_ranks_all_chem <- AI_ranks_all_chem %>%
    mutate(perc_total = total_lbs / total_lbs_all_chem)

AI_ranks_bee_chem <- bee_buf2 %>%
    group_by(year, buff_size, site_code, chem_code) %>%
    dplyr::summarise(total_lbs = sum(lbs_chem), total_kgs = lb_to_kg * sum(lbs_chem))
AI_ranks_bee_chem <- collect(AI_ranks_bee_chem)
AI_ranks_bee_chem <- AI_ranks_bee_chem %>%
    filter(site_code %in% as.numeric(crop_lib_attr$site_code)) %>%
    group_by(year, buff_size, chem_code) %>%
    dplyr::summarise(total_lbs = sum(total_lbs),
        total_kgs = lb_to_kg * sum(lbs_chem))
AI_ranks_bee_chem <- AI_ranks_bee_chem %>%
    group_by(year, buff_size, chem_code) %>%
    arrange(desc(total_lbs))
AI_ranks_bee_chem$chem_code <- as.character(AI_ranks_bee_chem$chem_code)
AI_ranks_bee_chem <- left_join(AI_ranks_bee_chem, pur_chem,
                               by = "chem_code")
total_lbs <- AI_ranks_bee_chem %>%
    group_by(year, buff_size) %>%
    dplyr::summarise(total_lbs_bee_chem = sum(total_lbs),
    total_kgs_bee_chem = lb_to_kg * sum(total_lbs))
AI_ranks_bee_chem <- left_join(AI_ranks_bee_chem, total_lbs,
                               by = c("year", "buff_size"))
AI_ranks_bee_chem <- AI_ranks_bee_chem %>%
    mutate(perc_total = total_lbs / total_lbs_bee_chem)

### get some stats on which sites recieved most lbs chem
### in each buffer size in each year.
perc_chem_use_by_site_site <- bee_buf2 %>%
    group_by(year, buff_size, site_code) %>%
    dplyr::summarise(total_lbs_site = sum(lbs_chem),
    total_kgs_site = lb_to_kg * sum(lbs_chem))
perc_chem_use_by_site_tot <- bee_buf2 %>%
    group_by(year, buff_size) %>%
    dplyr::summarise(total_lbs = sum(lbs_chem), total_kgs = lb_to_kg * sum(lbs_chem))
perc_chem_use_by_site <- left_join(perc_chem_use_by_site_site,
                                   perc_chem_use_by_site_tot, by = c("year", "buff_size"), copy = TRUE)
perc_chem_use_by_site <- collect(perc_chem_use_by_site)
perc_chem_use_by_site <- perc_chem_use_by_site %>%
    dplyr::mutate(percent_use = total_lbs_site / total_lbs) %>%
    group_by(year, buff_size) %>%
    arrange(desc(percent_use))
perc_chem_use_by_site$site_code <- as.character(perc_chem_use_by_site$site_code)
perc_chem_use_by_site <- left_join(perc_chem_use_by_site, pur_site, by = c("site_code"))
perc_chem_use_by_site <- left_join(perc_chem_use_by_site, crop_lib, by = c("site_code", "site_name"))

### site frequencies

site_freq <- ldply(1991:2013, function(Y){
    comtrs <- bee_buf_bearing %>% filter(year == Y) %>% select(comtrs)
    bee_buf <- bee_buf %>% filter(year == Y)
    bee_buf <- collect(bee_buf)
    bee_buf <- bee_buf %>% filter(comtrs %in% comtrs$comtrs)
    site_freq_tot <- bee_buf %>% ## get total number of comtrs
        group_by(year) %>%
            dplyr::select(comtrs) %>%
                dplyr::distinct(comtrs) %>%
                    dplyr::summarise(total_comtrs = n())
    site_freq <- bee_buf %>% ## get number of comtrs appearing for each site in each buffer, year
        group_by(year, buff_size, site_code) %>%
            dplyr::select(comtrs) %>%
                dplyr::distinct(comtrs) %>%
                    dplyr::summarise(total_comtrs_site = n())
    ## bring them together
    site_freq <- left_join(site_freq, site_freq_tot, by = "year")
    ## mutate into percentage
    site_freq <- site_freq %>%
        mutate(perc_freq = total_comtrs_site / total_comtrs)    
}, .progress = "text")
site_freq <- site_freq %>%
    mutate(site_code = as.character(site_code))
## join in other useful information (site_name)
site_freq <- left_join(site_freq, pur_site, by = "site_code")

##################################################################
##################################################################
## comtrs summaries for Christopher:

comtrs_summs <- ldply(1991:2013, function(Y){
    comtrs_bearing <- bee_buf_bearing %>%
                      filter(year == Y) %>%
                      select(comtrs) %>%
                      distinct(comtrs)
    comtrs_summs <- bee_buf %>%
                    filter(year == Y)
    comtrs_summs <- collect(comtrs_summs)
    comtrs_summs <- comtrs_summs %>%
                    filter(comtrs %in% comtrs_bearing$comtrs)
    comtrs_summs <- left_join(comtrs_summs, tox, by = "chem_code")
    comtrs_summs <- comtrs_summs %>%
                    dplyr::mutate(LD_pres = 453.592 * lbs_chem /
                                  (LD50..ug.bee.1. / (10^6))) %>%
                    group_by(year, comtrs, buff_size) %>%
                    dplyr::summarise(
                        total_lbs = sum(lbs_chem),
			total_kgs = lb_to_kg * sum(lbs_chem),
                        total_LDs = sum(LD_pres)
                        ) %>%
                    ungroup()
    comtrs_summs
}, .progress = "text")

save(AI_ranks_all_chem, AI_ranks_bee_chem,
     count_proximity_bee_crops, perc_chem_use_by_site,
     pur_chem, pur_site, site_chem, site_freq,
     yearly_chem_all_crops, yearly_chem_by_crop,
     site_prod, comtrs_summs, count_proximity_all_crops,
     file = "Bee_Data/YA_bee_analysis.rda")

