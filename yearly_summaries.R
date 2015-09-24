library(RPostgreSQL)
library(DBI)
library(dplyr)
library(ggplot2)
library(plyr)
library(reshape)

##setwd("~/Documents/ZhangLab/")

source("~/R/helper_functions.R")

con <- src_postgres(dbname="california",
                  host="ziram.lawr.ucdavis.edu",
                   user="jonathan"
                   ## ,options="-c search_path=bee"
                    )

## get bee info
bee_buf2 <- tbl(con, dplyr::sql("SELECT * FROM bee.almond_buff_chem_use_bearing_sum"))

## get toxicity info
tox <- read.csv("~/Bee_Data/AI-BeeToxicity.csv", stringsAsFactors = FALSE)

## get crop info
pur_site <- tbl(con, dplyr::sql("SELECT * FROM pur.dpr_site"))
pur_site <- collect(pur_site)

## get ai info
ai_use_type <- tbl(con, dplyr::sql("SELECT * FROM pur.dpr_ai_categories"))
ai_use_type <- collect(ai_use_type)


yearly_sums <- ldply(1991:2013, function(x){
    d <- bee_buf2 %>% filter(year == x)
    d <- collect(d)
    d <- left_join(d, tox, by = "chem_code")
    d %>%
        dplyr::mutate(num_lds = 453.592 * lbs_chem /
                      (LD50..ug.bee.1. / (10^6))) %>%
        group_by(year, buff_size) %>%
        dplyr::summarise(total_chem = sum(lbs_chem, na.rm = TRUE),
                         total_lds = sum(num_lds, na.rm = TRUE),
                         median_chem = median(lbs_chem, na.rm = TRUE),
                         median_lds = median(num_lds, na.rm = TRUE),
                         mean_chem = mean(lbs_chem, na.rm = TRUE),
                         mean_lds = mean(num_lds, na.rm = TRUE)
                         )
})

yearly_sums_AGO <- ldply(1991:2013, function(x){
    d <- bee_buf2 %>% filter(year == x)
    d <- collect(d)
    d <- left_join(d, tox, by = "chem_code")
    d %>%
        dplyr::mutate(num_lds = 453.592 * lbs_chem /
                      (LD50..ug.bee.1. / (10^6))) %>%
        group_by(year, buff_size, aer_gnd_ind) %>%
        dplyr::summarise(total_chem = sum(lbs_chem, na.rm = TRUE),
                         total_lds = sum(num_lds, na.rm = TRUE),
                         median_chem = median(lbs_chem, na.rm = TRUE),
                         median_lds = median(num_lds, na.rm = TRUE),
                         mean_chem = mean(lbs_chem, na.rm = TRUE),
                         mean_lds = mean(num_lds, na.rm = TRUE)
                         )
})

yearly_sums_ai_class <- ldply(1991:2013, function(x){
    d <- bee_buf2 %>% filter(year == x)
    d <- collect(d)
    d <- left_join(d, tox, by = "chem_code")
    d <- left_join(d, ai_use_type %>% select(chem_code, ai_class), by = "chem_code")
    d %>%
        dplyr::mutate(num_lds = 453.592 * lbs_chem /
                      (LD50..ug.bee.1. / (10^6))) %>%
        group_by(year, buff_size, ai_class) %>%
        dplyr::summarise(total_chem = sum(lbs_chem, na.rm = TRUE),
                         total_lds = sum(num_lds, na.rm = TRUE),
                         median_chem = median(lbs_chem, na.rm = TRUE),
                         median_lds = median(num_lds, na.rm = TRUE),
                         mean_chem = mean(lbs_chem, na.rm = TRUE),
                         mean_lds = mean(num_lds, na.rm = TRUE)
                         )
})

yearly_sums_AGO_ai_class <- ldply(1991:2013, function(x){
    d <- bee_buf2 %>% filter(year == x)
    d <- collect(d)
    d <- left_join(d, tox, by = "chem_code")
    d <- left_join(d, ai_use_type %>% select(chem_code, ai_class), by = "chem_code")
    d %>%
        dplyr::mutate(num_lds = 453.592 * lbs_chem /
                      (LD50..ug.bee.1. / (10^6))) %>%
        group_by(year, buff_size, aer_gnd_ind, ai_class) %>%
        dplyr::summarise(total_chem = sum(lbs_chem, na.rm = TRUE),
                         total_lds = sum(num_lds, na.rm = TRUE),
                         median_chem = median(lbs_chem, na.rm = TRUE),
                         median_lds = median(num_lds, na.rm = TRUE),
                         mean_chem = mean(lbs_chem, na.rm = TRUE),
                         mean_lds = mean(num_lds, na.rm = TRUE)
                         )
})


yearly_sums_by_site <- ldply(1991:2013, function(x){
    d <- bee_buf2 %>% filter(year == x)
    d <- collect(d)
    d <- left_join(d, tox, by = "chem_code")
    d %>%
        dplyr::mutate(num_lds = 453.592 * lbs_chem /
                      (LD50..ug.bee.1. / (10^6))) %>%
        group_by(year, buff_size, site_code) %>%
        dplyr::summarise(total_chem = sum(lbs_chem, na.rm = TRUE),
                         total_lds = sum(num_lds, na.rm = TRUE),
                         median_chem = median(lbs_chem, na.rm = TRUE),
                         median_lds = median(num_lds, na.rm = TRUE),
                         mean_chem = mean(lbs_chem, na.rm = TRUE),
                         mean_lds = mean(num_lds, na.rm = TRUE)
                         )
})

yearly_sums <- melt(yearly_sums, id = c("year","buff_size"))
yearly_sums_AGO <- melt(yearly_sums_AGO, id = c("year","buff_size", "aer_gnd_ind"))

yearly_sums_ai_class <- melt(yearly_sums_ai_class, id = c("year","buff_size", "ai_class"))
yearly_sums_AGO_ai_class <- melt(yearly_sums_AGO_ai_class, id = c("year","buff_size", "aer_gnd_ind", "ai_class"))


ggplot(yearly_sums, aes(x = year, y = value,
                        group = variable, color = variable)) +
    geom_line() +
    facet_grid(variable ~ buff_size, scale = "free")

ranks <- yearly_sums_by_site %>%
    filter(year == 2013 & buff_size == 5)

yearly_sums_by_site <- melt(yearly_sums_by_site,
                            id = c("year","buff_size", "site_code"))

yearly_sums_by_site <- left_join(yearly_sums_by_site, pur_site,
                                 by = "site_code")

r1 <- ranks %>%
    arrange(desc(total_lds)) %>%
    head()

r2 <- site_freq %>%
    filter(year == 2013 & buff_size == 5) %>%
    arrange(desc(perc_freq)) %>%
    head()

ggplot(yearly_sums_by_site %>%
       filter(site_code %in% as.numeric(r2$site_code)),
       aes(x = year, y = value, group = site_name, color = hf$removeParens(site_name))) +
    geom_line() +
    facet_grid(variable ~ buff_size, scales = "free")


save(yearly_sums, yearly_sums_by_site, yearly_sums_AGO,
     yearly_sums_ai_class, yearly_sums_AGO_ai_class,
     file = "~/Bee_Data/yearly_sums.rda")
