library(RPostgreSQL)
library(DBI)
library(dplyr)
library(ggplot2)
library(plyr)
library(stringdist)


### load data, functions, etc.

## get hf (helper functions)
source("~/R/helper_functions.R")
prods_lookup <- read.csv("~/comparisons.csv", na.strings = "", stringsAsFactors = FALSE)

con <- src_postgres(dbname="california",
                  host="ziram.lawr.ucdavis.edu",
                   user="jonathan"
                   ## ,options="-c search_path=bee"
                   )

pur_chem <- tbl(con, dplyr::sql("SELECT * FROM pur.chemical"))
pur_prod <- tbl(con, dplyr::sql("SELECT * FROM pur.product"))
pur_udc <- tbl(con, dplyr::sql("SELECT DISTINCT prodno, chem_code, prodchem_pct FROM pur.udc"))

pur_chem <- collect(pur_chem)
pur_prod <- collect(pur_prod)
pur_udc <- collect(pur_udc)

prods <- ldply(1:nrow(prods_lookup), function(k){ ##
    row <- prods_lookup[k,]
    prod <- row$product
    if(is.na(row$confirm)){
        return(data.frame(original = prod,
                          product_name = row$guess,
                          prodno = NA, density = NA, stringsAsFactors = FALSE, check = "z"))
    }
    prod_possibilities <- grep(prod, pur_prod$product_name, ignore.case = TRUE, value = TRUE)
    i <- which.min(stringdist(toupper(prod), prod_possibilities))
    if(length(i) == 0){
        return(data.frame(original = prod, product_name = NA, prodno = NA, density = NA, stringsAsFactors = FALSE, check = "x"))
    }
    ## chem_possibilities <- grep(prod, pur_chem$chemname, ignore.case = TRUE, value = TRUE)
    ## j <- which.min(stringdist(toupper(prod), chem_possibilities))
    matches <- pur_prod %>%
        filter(product_name == prod_possibilities[i]) %>%
            dplyr::mutate(jd = julian(as.Date(as.character(lastup_dt), "%d%m%Y"))) %>%
                dplyr::select(product_name, prodno, density, jd)
    if(any(!is.na(matches$jd))){
        i <- which.max(matches$jd)
        return(data.frame(original = prod, matches[i,-4], stringsAsFactors = FALSE, check = "y"))
    } else {
        return(data.frame(original = prod, matches[1, -4], stringsAsFactors = FALSE, check = "y"))
    }
})

save(prods, pur_chem, pur_prod, pur_udc, file = "prodInfo.rda")

