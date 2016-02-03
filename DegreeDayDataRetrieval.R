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

## use postGIS to do the dirty work of finding
## the nearest station to each bearing almon section
comtrs_station <- collect(tbl(con, dplyr::sql("SELECT DISTINCT ON(b.co_mtrs)
                                       b.co_mtrs AS comtrs, c.station_num AS station_num, ST_Distance(b.geom, c.geom) AS dist
                                       FROM boundaries.mtrs b, cimis.sites c
                                       WHERE b.co_mtrs IN (SELECT DISTINCT comtrs FROM bee.almond_sections_bearing)
                                       ORDER BY b.co_mtrs, b.geom <-> c.geom")))

## if need be, get all relevant chem_codes from here
chem <- collect(tbl(con, dplyr::sql("SELECT DISTINCT chem_code as code FROM
                                bee.almond_buff_chem_use_4_sum")))

## use this to single out the bearing almond comtrs
bearing <- collect(tbl(con, dplyr::sql("SELECT DISTINCT comtrs FROM
                                                bee.almond_sections_bearing")))

## cimis daily data gives us the date on which daily max temps first rose to 55degF for each station.
station_dates <- collect(tbl(con, dplyr::sql(
    "SELECT MIN(julian_date) as jd, MIN(date) as date, year, station_num FROM
     (
      SELECT date, station_num, date_part('year', date) AS year, julian_date FROM cimis.daily
      WHERE
        day_air_tmp_max >= 12.7778 AND date_part('year', date) > 1999 AND date_part('year', date) < 2014 AND
        date_part('month', date) > 1 AND date_part('month', date) < 4
     ) a
     GROUP BY year, station_num"
    )))


tmp <- left_join(comtrs_station, station_dates, by = "station_num")

## lapply(unique(tmp$comtrs), function(comtrs){
##     paste0("SELECT * FROM pur.udc
##      WHERE comtrs IN (SELECT DISTINCT b.co_mtrs as comtrs FROM boundaries.mtrs a. boundaries.mtrs b
##                       WHERE a.co_mtrs = '", comtrs,"' AND ST_Distance(a.geom, b.geom) <= 1609.34)")
## })

## tmp <- collect(tbl(con, dplyr::sql("SELECT DISTINCT b.co_mtrs as comtrs FROM boundaries.mtrs a, boundaries.mtrs b
##                       WHERE a.co_mtrs = '01M02S02E15' AND ST_Distance(a.geom, b.geom) <= 100")))

## talk to christopher about optimal way of getting the
## right data. reminder: see org file.

## For each comtrs, find the date at which the daily max
## temp at the station nearest it begins to exceed 55degF

## 55 deg F = 12.7778 deg C


