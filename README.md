[![Build Status](https://travis-ci.org/dleutnant/influxdbr.svg?branch=master)](https://travis-ci.org/dleutnant/influxdbr)
# influxdbr

## see branch 'dev' for latest dev version!!! InfluxDB V1.2 ready!

R interface to InfluxDB (>=0.9.3, 0.9.4 diagnotics already included)

Install using devtools:
```
if (!require(devtools))
  install.packages('devtools')
devtools::install_github("dleutnant/influxdbr")
```

Example use:

```
# load libs
library(xts)
library(influxdbr)
```

```
# attach data "sample_matrix"
data("sample_matrix")

# create xts object
xts_data <- xts::as.xts(x = sample_matrix)

# assign some attributes
xts::xtsAttributes(xts_data) <- list(info="SampleDataMatrix",
                                     UnitTesting=TRUE, 
                                     n=180)
                                     
str(xts_data)
An ‘xts’ object on 2007-01-02/2007-06-30 containing:
  Data: num [1:180, 1:4] 50 50.2 50.4 50.4 50.2 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:4] "Open" "High" "Low" "Close"
  Indexed by objects of class: [POSIXct,POSIXt] TZ: 
  xts Attributes:  
List of 3
 $ info       : chr "SampleDataMatrix"
 $ UnitTesting: logi TRUE
 $ n          : num 180
```

```
# create influx connection object
con <- influxdbr::influx_connection(host = "localhost",
                                      port = 8086,
                                      user = "root",
                                      pass = "root")
success: (204) No Content

# create new database
influxdbr::create_database(con = con, db = "mydb")

influxdbr::show_databases(con = con)
[1] "mydb" 

# write data to database
influxdbr::influx_write(con = con, 
                        db = "mydb",
                        xts = xts_data, 
                        measurement = "sampledata")

# show measurements
influxdbr::show_measurements(con = con, db = "mydb")
[1] "sampledata"

# select values
result <- influx_select(con = con, db = "mydb", value = "Open, High", 
                        from = "sampledata", limit = 10, order_desc = TRUE)
result
$sampledata
                        Open
2007-06-20 22:00:00 47.71012
2007-06-21 22:00:00 47.56849
2007-06-22 22:00:00 47.22873
2007-06-23 22:00:00 47.23996
2007-06-24 22:00:00 47.20471
2007-06-25 22:00:00 47.44300
2007-06-26 22:00:00 47.62323
2007-06-27 22:00:00 47.67604
2007-06-28 22:00:00 47.63629
2007-06-29 22:00:00 47.67468

$sampledata
                        High
2007-06-20 22:00:00 47.71012
2007-06-21 22:00:00 47.59266
2007-06-22 22:00:00 47.24771
2007-06-23 22:00:00 47.30287
2007-06-24 22:00:00 47.42772
2007-06-25 22:00:00 47.61611
2007-06-26 22:00:00 47.71673
2007-06-27 22:00:00 47.70460
2007-06-28 22:00:00 47.77563
2007-06-29 22:00:00 47.94127

# show tag keys
show_tag_keys(con = con, db = "mydb")
$sampledata
[1] "UnitTesting" "info"        "n" 

# show tag values
show_tag_values(con = con, db = "mydb", from = NULL, key = "UnitTesting")
[1] TRUE
