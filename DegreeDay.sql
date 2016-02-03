DROP TABLE bee.degreeStation;
CREATE TABLE bee.degreeStation AS
SELECT cs.station_num,
       jd, comtrs,
       date, year
FROM
(
	SELECT DISTINCT ON(b.co_mtrs)
 	      b.co_mtrs AS comtrs,
 	      c.station_num AS station_num,
       	      ST_Distance(b.geom, c.geom) AS dist
	FROM boundaries.mtrs b, cimis.sites c
	WHERE b.co_mtrs IN
	(
		SELECT DISTINCT comtrs FROM bee.almond_sections_bearing
	)
	ORDER BY b.co_mtrs, b.geom <-> c.geom
) cs LEFT JOIN
(
	SELECT MIN(julian_date) as jd,
       	       MIN(date) as date, year,
       	       station_num
	FROM
	(
		SELECT date, station_num,
		       date_part('year', date) AS year,
		       julian_date
		FROM cimis.daily
		WHERE day_air_tmp_max >= 12.7778 AND
	  	      date_part('year', date) > 1999 AND
	  	      date_part('year', date) < 2014 AND
            	      date_part('month', date) > 1 AND
	    	      date_part('month', date) < 4
         ) a GROUP BY year, station_num
) sd ON cs.station_num = sd.station_num;
