## figure out how to connect to db, then
## do some summary statistics
setwd("/Users/Yoni/Documents/ZhangLab")

library(dplyr)
library(RPostgreSQL)
library(DBI)
library(ggplot2)
library(plyr)
library(reshape)
library(wesanderson)

## source("R/helper_functions.R") ## local machine
source("~/R/helper_functions.R")
## Gives: hf (helper functions)


con <- src_postgres(dbname="california",
                  host="ziram.lawr.ucdavis.edu",
                   user="jonathan"
                   ## ,options="-c search_path=bee"
                   )

bee_buf1 <- tbl(con, dplyr::sql("SELECT * FROM bee.almond_buff_chem_use_4_sum"))
bee_buf2 <- tbl(con, dplyr::sql("SELECT * FROM bee.almond_buff_chem_use"))

## to get crop info
pur_site <- tbl(con, dplyr::sql("SELECT * FROM pur.dpr_site"))
pur_site <- collect(pur_site)
## to get chem info
pur_chem <- tbl(con, dplyr::sql("SELECT * FROM pur.chemical"))
pur_chem <- collect(pur_chem)
## to get toxicity info
## tox <- read.csv("BeePUR/AI-BeeToxicity.csv") ## local machine
tox <- read.csv("~/Bee_Data/AI-BeeToxicity.csv") ## abound machine


## lets look at 2013 for now, figure out some trends
## / what we even want to do, 
y2013_1 <- bee_buf1 %>%
    filter(year == 2013)
y2013_2 <- bee_buf2 %>%
    filter(year == 2013)

y2013_1 <- collect(y2013_1)
y2013_2 <- collect(y2013_2)


y2013 <- y2013 %>% mutate(lbs_per_acre_treated = lbs_chem / acre_treated,
                          lbs_per_acre_planted = lbs_chem / acre_planted,
                          lbs_per_total_acre = lbs_chem / 640,
                          total_acreage_of_buffer = 640*(pi*(buff_size + .5)^2)
                          )

y2013 <- collect(y2013)

## this is wrong because each row represents an application
## which allows for repeats in the acre_planted field whenever
## a section was hit by chemicals more than once
d <- bee_buf %>%
    group_by(year, comtrs, buff_size) %>%
    dplyr::summarise(
        total_lbs = sum(lbs_chem),
        num_air_applications = sum(aer_gnd_ind == "A"),
        num_gnd_applications = sum(aer_gnd_ind == "G")
        ) %>%
    ungroup() %>%
    dplyr::mutate(lbs_acre = total_lbs / (640 * pi* (buff_size + .5)^2 - 640))
d <- collect(d)

res <- y2013 %>%
    group_by(comtrs, buff_size) %>%
    dplyr::summarise(
        ## total lbs of all chemicals applied
        ## within the given buffer of a section
        total_chem_lbs = sum(lbs_chem, na.rm = TRUE),
        ## total acres treated within the given
        ## buffer size of the sections
        total_acre_treated = sum(acre_treated, na.rm = TRUE),
        ## total acres planted within the given
        ## buffer size of the section
        total_acre_planted = sum(acre_planted, na.rm = TRUE),
        ## mean lbs of chemical used within the
        ## given buffer of the section
        mn_lbs_chem  = mean(lbs_chem, na.rm = TRUE),
        ## variance of lbs of chemical used with
        ## the given buffer of the section
        var_lbs_chem = var(lbs_chem, na.rm = TRUE),
        ## most frequently occuring crop within
        ## the given buffer of the section
        most_freq_crop = as.numeric(
            names(table(site_code))[which.max(table(site_code))]
            ),
        ## most frequently occuring chemical within
        ## the given buffer of the section
        most_freq_chem = as.numeric(
            names(table(chem_code))[which.max(table(chem_code))]
            ),
        ## total number of applications made within
        ## the given buffer of the section
        num_applications = n()
        ) %>%
    ungroup() %>%
    dplyr::mutate(
        ## ratio of total lbs applied to total acres planted
        ## within the given buffer of the section
        tot_lbs_per_acre_planted = total_chem_lbs / total_acre_planted,
        ## ratio of total lbs applied to total acre planted within
        ## the given buffer of the section
        tot_lbs_per_acre_treated = total_chem_lbs / total_acre_treated,
        ## also include the percent of the surrounding buffer
        ## that has been planted:
        percent_buff_planted = total_acre_planted / (640 * pi* (buff_size + .5)^2 - 640)
        )

res2 <- y2013 %>%
    ## group by section, buffer size, and chemical
    group_by(comtrs, buff_size, chem_code) %>%
    dplyr::summarise(
        ## find total amount of that chemical applied in
        ## that buffer of the section
        total_lbs_chem = sum(lbs_chem)
        ) %>%
    ungroup() %>%
    ## regroup by section and buffer size
    group_by(comtrs, buff_size) %>%
    dplyr::summarise(
        ## find the chemical most applied by total volume
        most_lbs_chem = chem_code[which.max(total_lbs_chem)],
        ## find the number of chemicals applied within the
        ## given buffer of the section
        num_chem_applied_in_buffer = n()
        )

res3 <- y2013 %>%
        ## group by section, buffer, and crop
        group_by(comtrs, buff_size, site_code) %>%
        dplyr::summarise(
        ## find the total lbs of all chemicals applied
        ## to the given crop within the given buffer of
        ## the section
            total_lbs_site = sum(lbs_chem)) %>%
        ungroup() %>%
        ## regroup by section and buffer
        group_by(comtrs, buff_size) %>%
        ## resummarize by choosing the crop that is most
        ## often treated
        dplyr::summarise(most_treated_site = site_code[which.max(total_lbs_site)])

## comtrs threats
ct <- left_join(left_join(res, res2, by = c("comtrs", "buff_size")), res3, by = c("comtrs", "buff_size"))
                          
ggplot(ct[1:100,], aes(x = comtrs, y = num_applications)) +
    geom_point()
    ## geom_pointrange(aes(ymax = av_lbs_chem + var_lbs_chem,
    ##                     ymin = av_lbs_chem - var_lbs_chem))

## need to incorporate buffer information


## covariance of crop with lbs/acre_treated & crop with lbs/acre_planted
qplot(x = y2013$acre_treated, geom = "density")
qplot(x = y2013$acre_planted, geom = "density")


y2013$lbs_per_acre_planted <- unlist(llply(y2013$lbs_per_acre_planted, function(x){
    if(is.infinite(x)){return(0)} else {return(x)}
}))

cvs <- dlply(y2013, .(buff_size), function(d){
    cov_site_meth <- cov(d$site_code, d$aer_gnd_ind)
})

ms <- ddply(y2013, .(buff_size), function(d){
    d$site_code <- as.character(d$site_code)
    m_tre <- lm(lbs_per_acre_treated ~ site_code - 1, data = d)
    m_pla <- lm(lbs_per_acre_planted ~ site_code - 1, data = d)
    conf_tre <- confint(m_tre)
    conf_pla <- confint(m_pla)
    coef_tre = coef(m_tre)
    data.frame(
        site_code = names(coef_tre),
        coef_tre = coef_tre,
        coef_pla = coef(m_pla),
        conf_tre = conf_tre,
        conf_pla = conf_pla,
        stringsAsFactors = FALSE)
}, .progress = "text") 

ggplot(ms, aes(x = site_code, y = coef_tre)) +
    geom_point() +
    geom_pointrange(aes(ymax = conf_tre.97.5.., ymin = conf_tre.2.5..)) +
    facet_wrap(~buff_size, scale = "free") +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank())

ggplot(y2013, aes(x = as.factor(site_code),
                 y = lbs_per_acre_treated,
                 group = as.factor(site_code))) +
    geom_boxplot() +
    facet_wrap(~buff_size, scale = "free") +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank())

y2013_iless <- y2013[-i,]

ggplot(y2013_iless, aes(x = as.factor(site_code),
                 y = lbs_per_acre_treated,
                 group = as.factor(site_code))) +
    geom_boxplot() +
    facet_wrap(~buff_size, scale = "free") +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank())


## weird data points ##
k <- which.max(y2013$acre_treated)
m <- which.max(y2013$acre_planted) 
i <- which(y2013$lbs_per_acre_treated == max(y2013$lbs_per_acre_treated, na.rm = TRUE)) 
j <- which.max(y2013$lbs_per_acre_planted)

tmp <- y2013[c(i,j,k,m),]
##___________________##


## relationship between site_code and chem_code
## chem_codes <- as.character(unique(y2013$chem_code))

## ## not sure why I chose ddply - I think out of fear
## ## that geom tile would break if all chemicals didnt
## ## appear within each site group
## site_chem <- ddply(y2013, .(site_code), function(x){
##     tb <- table(x$chem_code)
##     unseen_chems <- setdiff(chem_codes, names(tb))
##     n_tb <- c(tb, rep(0, length(unseen_chems)))
##     names(n_tb) <-  c(names(tb), unseen_chems)
##     as.data.frame(as.list(n_tb))
## })
## site_chem$site_code <- as.character(site_chem$site_code)
## site_chem <- melt(site_chem, id = "site_code")

## ggplot(site_chem,
##        aes(x = site_code, y = variable, fill = binned)) +
##     geom_tile() +
##     theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 5))

## ggplot(site_chem %>%
##        filter(value < 10000),
##        aes(x = site_code, y = variable)) +
##     geom_tile(aes(fill = value))

## bins <- c(hf$SegmentVec(0:49, 5), hf$SegmentVec(50:100, 2), list(101:250), list(251:500), list(501:2000), list(2001:max(site_chem$value)))

## site_chem$binned <- unlist(llply(site_chem$value, function(x){
##     i <- unlist(llply(seq_along(bins), function(y){
##         x %in% bins[[y]]
##     }))
##     which(i)
## }))

## now lets look at the correlation not by count, but by amount of chem
## used in each application, total chem used, lbs/area treated, lbs/area planted.

## include buffer size, in grouping, because if we look across all buffers,
## we'll be getting a lot of repeats -> buff = 5 will give us total for
## buff sizes 1-5.
site_chem <- y2013 %>%                            ## TODO:
    group_by(site_code, buff_size, chem_code) %>% ## add in comtrs later
    dplyr::summarise(mn_lbs_acre_treated = mean(lbs_per_acre_treated,
                         na.rm = TRUE),
                     mn_lbs_acre_planted = mean(lbs_per_acre_planted,
                         na.rm = TRUE),
                     mn_lbs_total_acre  = mean(lbs_per_total_acre,
                         na.rm = TRUE),
                     acres_treat_over_total_acre = sum(acre_treated /
                         total_acreage_of_buffer,
                         na.rm = TRUE),
                     num_applications = n(),
                     total_lbs = sum(lbs_chem, na.rm = TRUE),
                     num_air_applications = sum(aer_gnd_ind == "A", na.rm = TRUE),
                     num_gnd_applications = sum(aer_gnd_ind == "G", na.rm = TRUE)
                     ) %>%
    ungroup() %>%
    mutate(site_code = as.character(site_code),
           chem_code = as.character(chem_code))

## lets get an order:
order <- site_chem %>%
    group_by(site_code) %>%
    dplyr::summarise(r = sum(num_applications, na.rm = TRUE)) %>%
    arrange(desc(r))
order$rank <- rank(order$r)
order$site_ordered <- factor(order$site_code, levels = order$site_code)
site_chem <- left_join(site_chem, order, by = "site_code")


## total number of applications
ggplot(site_chem %>% filter(buff_size == 5),
       aes(x = site_ordered, y = chem_code, fill = num_applications)) +
    geom_tile() +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
    theme(axis.ticks = element_blank(), axis.text.y = element_blank())

## mean lbs per total acreage
ggplot(site_chem %>% filter(buff_size == 5),
       aes(x = site_ordered, y = chem_code, fill = mn_lbs_total_acre)) +
    geom_tile() +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
    theme(axis.ticks = element_blank(), axis.text.y = element_blank())

## percent of buffer treated
ggplot(site_chem %>% filter(buff_size == 5),
       aes(x = site_ordered, y = chem_code, fill = acres_treat_over_total_acre)) +
    geom_tile() +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
    theme(axis.ticks = element_blank(), axis.text.y = element_blank())

## total number of air applications
## these are important because even if the crop receiving the
## application is not one that attracts bees, pesticide drift
## could contaminate nearby bee-friendly flora
ggplot(site_chem %>% filter(buff_size == 5),
       aes(x = site_ordered, y = chem_code, fill = num_air_applications)) +
    geom_tile() +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
    theme(axis.ticks = element_blank(), axis.text.y = element_blank())



## check to see if negative site codes are legit
neg_site_codes <- tbl(con, dplyr::sql("SELECT site_code FROM pur.udc WHERE site_code < 0"))
neg_site_codes <- collect(neg_site_codes)
