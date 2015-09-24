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

tmp <- tbl(con, dplyr::sql("SELECT applic_cnt, date_part('year', applic_dt) as y, date_part('month', applic_dt) as m FROM pur.udc"))

tmp <- tbl(con, dplyr::sql("SELECT * FROM pur.udc WHERE date_part('year', applic_dt) = 2013 AND date_part('month', applic_dt) >= 2 AND date_part('month', applic_dt) <= 3"))

tmp <- tmp %>%
    dplyr::filter(m >= 2 & m <= 3 & y == 2013) %>%
    dplyr::summarise(n_apps = sum(applic_cnt))


tmp <- collect(tmp)

bee_buf2 <- tbl(con, dplyr::sql("SELECT COUNT(*) FROM bee.almond_buff_chem_use_bearing_sum WHERE buff_size = 5 AND year = 2013"))

