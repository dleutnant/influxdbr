[![Build Status](https://travis-ci.org/dleutnant/influxdbr.svg?branch=0.9.6)](https://travis-ci.org/dleutnant/influxdbr)
influxdbr
================

R interface to InfluxDB (&gt;=0.9.3, 0.10.3 compatible, diagnostics &gt;= 0.9.4)

Install using devtools:

``` r
if(!require(devtools)) {
  install.packages('devtools')
  devtools::install_github("dleutnant/influxdbr")
}
```

    ## Loading required package: devtools

Example use:

``` r
# load libs
library(xts)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library(influxdbr)
```

``` r
# attach data "sample_matrix"
data("sample_matrix")

# create xts object
xts_data <- xts::as.xts(x = sample_matrix)

# assign some attributes
xts::xtsAttributes(xts_data) <- list(info = "SampleDataMatrix",
                                     UnitTesting = TRUE, 
                                     n = 180)
                                     
str(xts_data)
```

    ## An 'xts' object on 2007-01-02/2007-06-30 containing:
    ##   Data: num [1:180, 1:4] 50 50.2 50.4 50.4 50.2 ...
    ##  - attr(*, "dimnames")=List of 2
    ##   ..$ : NULL
    ##   ..$ : chr [1:4] "Open" "High" "Low" "Close"
    ##   Indexed by objects of class: [POSIXct,POSIXt] TZ: 
    ##   xts Attributes:  
    ## List of 3
    ##  $ info       : chr "SampleDataMatrix"
    ##  $ UnitTesting: logi TRUE
    ##  $ n          : num 180

``` r
con <- influxdbr::influx_connection(host = "localhost",
                                      port = 8086,
                                      user = "root",
                                      pass = "root")
```

    ## Success: (204) No Content

``` r
influxdbr::create_database(con = con, db = "mydb")

influxdbr::show_databases(con = con)
```

    ## [1] "_internal" "mydb"

``` r
influxdbr::influx_write(con = con, 
                        db = "mydb",
                        xts = xts_data, 
                        measurement = "sampledata")
```

``` r
influxdbr::show_measurements(con = con, db = "mydb")
```

    ## [1] "sampledata"

``` r
result <- influx_select(con = con, 
                        db = "mydb", 
                        field_keys = "Open, High", 
                        measurement = "sampledata",
                        limit = 10, 
                        order_desc = TRUE)

print(result)
```

    ## [[1]]
    ## [[1]]$sampledata
    ##                Open
    ## 2007-06-21 47.71012
    ## 2007-06-22 47.56849
    ## 2007-06-23 47.22873
    ## 2007-06-24 47.23996
    ## 2007-06-25 47.20471
    ## 2007-06-26 47.44300
    ## 2007-06-27 47.62323
    ## 2007-06-28 47.67604
    ## 2007-06-29 47.63629
    ## 2007-06-30 47.67468
    ## 
    ## [[1]]$sampledata
    ##                High
    ## 2007-06-21 47.71012
    ## 2007-06-22 47.59266
    ## 2007-06-23 47.24771
    ## 2007-06-24 47.30287
    ## 2007-06-25 47.42772
    ## 2007-06-26 47.61611
    ## 2007-06-27 47.71673
    ## 2007-06-28 47.70460
    ## 2007-06-29 47.77563
    ## 2007-06-30 47.94127

``` r
show_tag_keys(con = con, db = "mydb")
```

    ## $sampledata
    ## [1] "UnitTesting" "info"        "n"

``` r
show_tag_values(con = con, db = "mydb", measurement = NULL, key = "UnitTesting")
```

    ##   UnitTesting
    ## 1        TRUE
