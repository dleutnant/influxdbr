influxdbr
================

[![Build Status](https://travis-ci.org/dleutnant/influxdbr.svg?branch=prepare_cran)](https://travis-ci.org/dleutnant/influxdbr)

R interface to InfluxDB (V1.2)

Install using devtools:

``` r
if (!require(devtools)) {
  install.packages('devtools')
}
```

    ## Loading required package: devtools

``` r
devtools::install_github("dleutnant/influxdbr@dev", quiet = TRUE)
```

Example use:
------------

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
# create connection object 
# (here: based on a config file with (s. package documentation))
con <- influxdbr::influx_connection(group = "admin")
```

    ## Success: (204) No Content

``` r
# create new database
influxdbr::create_database(con = con, db = "mydb")

# list all databases
influxdbr::show_databases(con = con)
```

    ## # A tibble: 11 x 1
    ##         name
    ##        <chr>
    ##  1 _internal
    ##  2    stbmod
    ##  3     wasig
    ##  4  wasig-fr
    ##  5   wasig-h
    ##  6      tmp2
    ##  7      tmp3
    ##  8       tmp
    ##  9      test
    ## 10    new_db
    ## 11      mydb

``` r
# write example xts-object to database
# attributes of the xts object are interpreted as InfluxDB tags (keys and values)
# colnames of the xts object are interpreted as InfluxDB fields (keys and values)
influxdbr::influx_write(con = con, 
                        db = "mydb",
                        xts = xts_data, 
                        measurement = "sampledata")
```

``` r
# check if measurement was succefully written
influxdbr::show_measurements(con = con, db = "mydb")
```

    ## # A tibble: 1 x 1
    ##         name
    ##        <chr>
    ## 1 sampledata

request series as tibbles
-------------------------

``` r
# return tibble
# fetch time series data by using the helper function `influx_select`
result <- influx_select(con = con, 
                        db = "mydb", 
                        field_keys = "Open, High", 
                        measurement = "sampledata",
                        group_by = "*",
                        limit = 10, 
                        order_desc = TRUE, 
                        return_xts = FALSE)

result
```

    ## [[1]]
    ## # A tibble: 10 x 9
    ##    statement_id series_names series_partial UnitTesting             info
    ##           <int>        <chr>          <lgl>       <chr>            <chr>
    ##  1            0   sampledata          FALSE        TRUE SampleDataMatrix
    ##  2            0   sampledata          FALSE        TRUE SampleDataMatrix
    ##  3            0   sampledata          FALSE        TRUE SampleDataMatrix
    ##  4            0   sampledata          FALSE        TRUE SampleDataMatrix
    ##  5            0   sampledata          FALSE        TRUE SampleDataMatrix
    ##  6            0   sampledata          FALSE        TRUE SampleDataMatrix
    ##  7            0   sampledata          FALSE        TRUE SampleDataMatrix
    ##  8            0   sampledata          FALSE        TRUE SampleDataMatrix
    ##  9            0   sampledata          FALSE        TRUE SampleDataMatrix
    ## 10            0   sampledata          FALSE        TRUE SampleDataMatrix
    ## # ... with 4 more variables: n <chr>, time <dttm>, Open <dbl>, High <dbl>

request series as xts
---------------------

``` r
# return xts object
# fetch time series data by using the helper function `influx_select`
result <- influx_select(con = con, 
                        db = "mydb", 
                        field_keys = "Open, High", 
                        measurement = "sampledata",
                        group_by =  "*",
                        limit = 10, 
                        order_desc = TRUE, 
                        return_xts = TRUE)

# InfluxDB tags are now xts attributes.
# Because xts objects are basically matrices (which can store one data type only), 
# a single xts object is created for each InfluxDB field. 
# This ensures a correct representation of the field values data type (instead of getting all into a "character" matrix).
str(result)
```

    ## List of 1
    ##  $ :List of 2
    ##   ..$ sampledata:An 'xts' object on 2007-06-20 22:00:00/2007-06-29 22:00:00 containing:
    ##   Data: num [1:10, 1] 47.7 47.6 47.2 47.2 47.2 ...
    ##  - attr(*, "dimnames")=List of 2
    ##   ..$ : NULL
    ##   ..$ : chr "Open"
    ##   Indexed by objects of class: [POSIXct,POSIXt] TZ: GMT
    ##   xts Attributes:  
    ## List of 6
    ##   .. ..$ statement_id  : int 0
    ##   .. ..$ series_names  : chr "sampledata"
    ##   .. ..$ series_partial: logi FALSE
    ##   .. ..$ UnitTesting   : chr "TRUE"
    ##   .. ..$ info          : chr "SampleDataMatrix"
    ##   .. ..$ n             : chr "180"
    ##   ..$ sampledata:An 'xts' object on 2007-06-20 22:00:00/2007-06-29 22:00:00 containing:
    ##   Data: num [1:10, 1] 47.7 47.6 47.2 47.3 47.4 ...
    ##  - attr(*, "dimnames")=List of 2
    ##   ..$ : NULL
    ##   ..$ : chr "High"
    ##   Indexed by objects of class: [POSIXct,POSIXt] TZ: GMT
    ##   xts Attributes:  
    ## List of 6
    ##   .. ..$ statement_id  : int 0
    ##   .. ..$ series_names  : chr "sampledata"
    ##   .. ..$ series_partial: logi FALSE
    ##   .. ..$ UnitTesting   : chr "TRUE"
    ##   .. ..$ info          : chr "SampleDataMatrix"
    ##   .. ..$ n             : chr "180"

simplify InfluxDB response
--------------------------

``` r
# In case the InfluxDB response is expected to be a single series only, 
# we can flatten the list ('simplifyList = TRUE') to directly get to the data.
# This enhances a pipeable work flow.
# fetch time series data by using the helper function `influx_select`
result <- influx_select(con = con, 
                        db = "mydb", 
                        field_keys = "Open", 
                        measurement = "sampledata",
                        group_by =  "*",
                        limit = 10, 
                        order_desc = TRUE, 
                        return_xts = TRUE, 
                        simplifyList = TRUE)

str(result)
```

    ## An 'xts' object on 2007-06-20 22:00:00/2007-06-29 22:00:00 containing:
    ##   Data: num [1:10, 1] 47.7 47.6 47.2 47.2 47.2 ...
    ##  - attr(*, "dimnames")=List of 2
    ##   ..$ : NULL
    ##   ..$ : chr "Open"
    ##   Indexed by objects of class: [POSIXct,POSIXt] TZ: GMT
    ##   xts Attributes:  
    ## List of 6
    ##  $ statement_id  : int 0
    ##  $ series_names  : chr "sampledata"
    ##  $ series_partial: logi FALSE
    ##  $ UnitTesting   : chr "TRUE"
    ##  $ info          : chr "SampleDataMatrix"
    ##  $ n             : chr "180"
