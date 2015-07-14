[![Build Status](https://travis-ci.org/dleutnant/influxdbr.svg?branch=master)](https://travis-ci.org/dleutnant/influxdbr)
# influxdbr
R interface to InfluxDB (>=0.9.0)

Supports both querying and writing time series data 

Install using devtools:
```
> if (!require(devtools))
    install.packages('devtools')
> devtools::install_github("dleutnant/influxdbr")
```
Example use:

```
### create connection object
# connection command
> con <- influxdbr::influx_connection(host = "localhost",
                                      port = 8086,
                                      user = "root",
                                      pass = "root")
success: (204) No Content

### querying time series data
# query command
> result <- influxdbr::influx_query(con = con,
                                    db = "mydb",
                                    query = "SELECT * FROM temperature limit 10")

# print output
> result
$temperature
$temperature$values
                    [,1]
2013-03-08 15:42:00 11.8
2013-03-08 15:43:00 11.9
2013-03-08 15:44:00 11.8
2013-03-08 15:45:00 11.9
2013-03-08 15:46:00 11.7
2013-03-08 15:47:00 11.8
2013-03-08 15:48:00 11.7
2013-03-08 15:49:00 11.7
2013-03-08 15:50:00 11.8
2013-03-08 15:51:00 11.8


### writing time series data
# rename columns of xts object
> colnames(result$temperature$values) <- "rawdata"

# write xts object to influxdb
> influxdbr::influx_write(con = con, 
                          db = "mydb",
                          xts = result$temperature$values, 
                          measurement = "temperature_new",
                          precision = "default")

### show all measurements in "mydb"
# show measurements
> influx_query(con = con, db = "mydb", query = "show measurements")
$measurements
                 name
1         temperature
2     temperature_new
```
