#### ON abound ####
### load libraries
library(RPostgreSQL)
library(DBI)
library(dplyr)
library(plyr)
library(foreach)
library(lubridate)
library(reshape2)

### load functions
source("~/R/bee_half_life/func.R")

## connect to the database
con <- src_postgres(dbname="california",
                  host="ziram.lawr.ucdavis.edu",
                   user="jonathan"
                   ## ,options="-c search_path=bee"
                    )

comtrs_station <- tbl(con, dplyr::sql("SELECT b.co_mtrs, c.station_num FROM boundaries.mtrs b,
                                       cimis.sites c ORDER BY b.geom <-> c.geom"))

bee_buf <- tbl(con, dplyr::sql("SELECT * FROM bee.almond_buff_chem_use_4_sum"))
bee_buf_bearing <- collect(tbl(con, dplyr::sql("SELECT * FROM bee.almond_sections_bearing")))
bee_buf2 <- tbl(con, dplyr::sql("SELECT * FROM bee.almond_buff_chem_use_bearing_sum"))



## 55 deg F = 12.7778 deg C


