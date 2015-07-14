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
# load libs
library(xts)
library(influxdbr)

# attach data "sample_matrix"
data("sample_matrix")

# create xts object
xts_data <- xts::as.xts(x = sample_matrix)

# assign attributes to xts object
xts::xtsAttributes(xts_data) <- list(info="SampelDataMatrix",
                                     UnitTesting=TRUE, 
                                     n=180)

# print structure 
str(xts_data)                                     
                  
# create influx connection object
con <- influxdbr::influx_connection(group = "admin")

# create new database
influxdbr::influx_query(con = con, db=NULL, query = "create database mydb")

# write data to database
influxdbr::influx_write(con = con, 
                        db = "mydb",
                        xts = xts_data, 
                        measurement = "sampledata")
                        
# show measurements
influxdbr::influx_query(con = con, db="mydb", query = "show measurements")

# query database
res <- influx_query(con = con,
                    db = "mydb",
                    query = "select * from sampledata")

```
