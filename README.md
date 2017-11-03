influxdbr
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/influxdbr)](https://cran.r-project.org/package=influxdbr)
[![Build
Status](https://travis-ci.org/dleutnant/influxdbr.svg?branch=master)](https://travis-ci.org/dleutnant/influxdbr)

R interface to [InfluxDB](https://docs.influxdata.com/influxdb)

This package allows you to fetch and write time series data from/to an
InfluxDB server. Additionally, handy wrappers for the Influx Query
Language (IQL) to manage and explore a remote database are provided.

## Installation

Installation is easy thanks to CRAN:

``` r
install.packages("influxdbr")
```

You can install the dev version from github with:

``` r
# install.packages("devtools")
devtools::install_github("dleutnant/influxdbr@dev")
```

## Example

This is a basic example which shows you how to communicate (i.e. query
and write data) with the InfluxDB server.

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(influxdbr)
library(xts)
#> Loading required package: zoo
#> 
#> Attaching package: 'zoo'
#> The following objects are masked from 'package:base':
#> 
#>     as.Date, as.Date.numeric
#> 
#> Attaching package: 'xts'
#> The following objects are masked from 'package:dplyr':
#> 
#>     first, last
```

Let’s create first some sample data from the xts package and assign
arbitrary attributes:

``` r
# attach data "sample_matrix"
data("sample_matrix")

# create xts object
xts_data <- xts::as.xts(x = sample_matrix)
#> Warning in strptime(xx, f <- "%Y-%m-%d %H:%M:%OS", tz = tz): unknown
#> timezone 'zone/tz/2017c.1.0/zoneinfo/Europe/Berlin'

# assign some attributes
xts::xtsAttributes(xts_data) <- list(info = "SampleDataMatrix",
                                     UnitTesting = TRUE, 
                                     n = 180)
                                     
# print structure to inspect the object
str(xts_data)
#> An 'xts' object on 2007-01-02/2007-06-30 containing:
#>   Data: num [1:180, 1:4] 50 50.2 50.4 50.4 50.2 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : NULL
#>   ..$ : chr [1:4] "Open" "High" "Low" "Close"
#>   Indexed by objects of class: [POSIXct,POSIXt] TZ: 
#>   xts Attributes:  
#> List of 3
#>  $ info       : chr "SampleDataMatrix"
#>  $ UnitTesting: logi TRUE
#>  $ n          : num 180
```

### InfluxDB connection

To connect to an InfluxDB server, we need a connection object. A
connection object can be created by providing usual server details (e.g.
`host`, `port`, …) or with help of a group file, which conveniently
holds all information for us (s. package documentation):

``` r
# create connection object 
# (here: based on a config file with group "admin" in it (s. package documentation))
con <- influx_connection(group = "admin")
#> Success: (204) No Content
```

The `influxdbr` package provides handy wrappers to manage a remote
InfluxDB:

``` r
# create new database
create_database(con = con, db = "mydb")

# list all databases
show_databases(con = con)
#> # A tibble: 7 x 1
#>        name
#>       <chr>
#> 1 _internal
#> 2    stbmod
#> 3     wasig
#> 4  wasig-fr
#> 5   wasig-h
#> 6      mydb
#> 7      test
```

### Write data

#### xts

Writing an xts-object to the server can be achieved with `influx_write`.
In this case, columnnames of the `xts` object are used as InfluxDB’s
field keys, `xts`’s coredata represent field values. Attributes are
preserved and written as tag keys and values, respectively.

``` r
# write example xts-object to database
influx_write(con = con, 
             db = "mydb",
             x = xts_data, 
             measurement = "sampledata_xts")
```

#### data.frame

Writing a data.frame (or tibble) to the server can also be achieved with
`influx_write`. In this case, we need to specify which columns of the
data.frame represent time and tags. Fields are automatically
determined.Each row represents a unique data point. `NA`’s are not
supported and need to be removed. Timestamps should be located in column
`time`.

Remember that time and tags are optional: InfluxDB uses the server’s
local nanosecond timestamp in UTC if the timestamp is not included with
the point.

``` r
# convert the existing xts-object to data.frame
df_data <- dplyr::bind_cols(time = zoo::index(xts_data), # timestamp
                            data.frame(xts_data)) %>% # coredata
  dplyr::mutate(info = "SampleDataMatrix", # add tag 'info'
                UnitTesting = TRUE, # add tag 'UnitTesting'
                n = row_number()) # add tag 'n'

df_data
#> # A tibble: 180 x 8
#>          time     Open     High      Low    Close             info
#>        <dttm>    <dbl>    <dbl>    <dbl>    <dbl>            <chr>
#>  1 2007-01-02 50.03978 50.11778 49.95041 50.11778 SampleDataMatrix
#>  2 2007-01-03 50.23050 50.42188 50.23050 50.39767 SampleDataMatrix
#>  3 2007-01-04 50.42096 50.42096 50.26414 50.33236 SampleDataMatrix
#>  4 2007-01-05 50.37347 50.37347 50.22103 50.33459 SampleDataMatrix
#>  5 2007-01-06 50.24433 50.24433 50.11121 50.18112 SampleDataMatrix
#>  6 2007-01-07 50.13211 50.21561 49.99185 49.99185 SampleDataMatrix
#>  7 2007-01-08 50.03555 50.10363 49.96971 49.98806 SampleDataMatrix
#>  8 2007-01-09 49.99489 49.99489 49.80454 49.91333 SampleDataMatrix
#>  9 2007-01-10 49.91228 50.13053 49.91228 49.97246 SampleDataMatrix
#> 10 2007-01-11 49.88529 50.23910 49.88529 50.23910 SampleDataMatrix
#> # ... with 170 more rows, and 2 more variables: UnitTesting <lgl>, n <int>

# write example data.frame to database
influx_write(con = con, 
             db = "mydb",
             x = df_data,
             time_col = "time", tag_cols = c("info", "UnitTesting", "n"),
             measurement = "sampledata_df")
```

We can now check if the time series were succefully written:

``` r
# check if measurements were succefully written
show_measurements(con = con, db = "mydb")
#> # A tibble: 3 x 1
#>             name
#>            <chr>
#> 1     sampledata
#> 2  sampledata_df
#> 3 sampledata_xts
```

### Query data

To query the database, two functions `influx_query` and `influx_select`
are available. `influx_select` wraps around `influx_query` and can be
useful for simple requests because it provides default query parameters.
The return type can be configured to be of class `tibble` or of class
`xts`.

#### Return tibbles

If `return_xts = FALSE` a list of tibbles per query statement is
returned. Each tibble contains columns with statement\_id,
series\_names, tags, time and fields.

``` r
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
#> [[1]]
#> # A tibble: 10 x 9
#>    statement_id series_names series_partial UnitTesting             info
#>           <int>        <chr>          <lgl>       <chr>            <chr>
#>  1            0   sampledata          FALSE        TRUE SampleDataMatrix
#>  2            0   sampledata          FALSE        TRUE SampleDataMatrix
#>  3            0   sampledata          FALSE        TRUE SampleDataMatrix
#>  4            0   sampledata          FALSE        TRUE SampleDataMatrix
#>  5            0   sampledata          FALSE        TRUE SampleDataMatrix
#>  6            0   sampledata          FALSE        TRUE SampleDataMatrix
#>  7            0   sampledata          FALSE        TRUE SampleDataMatrix
#>  8            0   sampledata          FALSE        TRUE SampleDataMatrix
#>  9            0   sampledata          FALSE        TRUE SampleDataMatrix
#> 10            0   sampledata          FALSE        TRUE SampleDataMatrix
#> # ... with 4 more variables: n <chr>, time <dttm>, Open <dbl>, High <dbl>
```

#### Return xts

If `return_xts = TRUE` a list of xts objects per query statement is
returned. Because xts objects are basically matrices (which can store
one data type only), a single xts object is created for each InfluxDB
field. This ensures a correct representation of the field values data
type (instead of getting all into a “character” matrix). InfluxDB tags
are now xts attributes.

``` r
# fetch time series data by using the helper function `influx_select`
result <- influx_select(con = con, 
                        db = "mydb", 
                        field_keys = "Open, High", 
                        measurement = "sampledata",
                        group_by =  "*",
                        limit = 10, 
                        order_desc = TRUE, 
                        return_xts = TRUE)

str(result)
#> List of 1
#>  $ :List of 2
#>   ..$ sampledata:An 'xts' object on 2007-06-25 22:00:00/2007-06-30 containing:
#>   Data: num [1:10, 1] 47.4 47.4 47.6 47.6 47.7 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : NULL
#>   ..$ : chr "Open"
#>   Indexed by objects of class: [POSIXct,POSIXt] TZ: GMT
#>   xts Attributes:  
#> List of 6
#>   .. ..$ statement_id  : int 0
#>   .. ..$ series_names  : chr "sampledata"
#>   .. ..$ series_partial: logi FALSE
#>   .. ..$ UnitTesting   : chr "TRUE"
#>   .. ..$ info          : chr "SampleDataMatrix"
#>   .. ..$ n             : chr "180"
#>   ..$ sampledata:An 'xts' object on 2007-06-25 22:00:00/2007-06-30 containing:
#>   Data: num [1:10, 1] 47.6 47.6 47.7 47.7 47.7 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : NULL
#>   ..$ : chr "High"
#>   Indexed by objects of class: [POSIXct,POSIXt] TZ: GMT
#>   xts Attributes:  
#> List of 6
#>   .. ..$ statement_id  : int 0
#>   .. ..$ series_names  : chr "sampledata"
#>   .. ..$ series_partial: logi FALSE
#>   .. ..$ UnitTesting   : chr "TRUE"
#>   .. ..$ info          : chr "SampleDataMatrix"
#>   .. ..$ n             : chr "180"
```

#### Simplify InfluxDB response

In case the InfluxDB response is expected to be a single series only, we
can flatten the list (`simplifyList = TRUE`) to directly get to the
data. This enhances a pipeable work flow.

``` r
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
#> An 'xts' object on 2007-06-25 22:00:00/2007-06-30 containing:
#>   Data: num [1:10, 1] 47.4 47.4 47.6 47.6 47.7 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : NULL
#>   ..$ : chr "Open"
#>   Indexed by objects of class: [POSIXct,POSIXt] TZ: GMT
#>   xts Attributes:  
#> List of 6
#>  $ statement_id  : int 0
#>  $ series_names  : chr "sampledata"
#>  $ series_partial: logi FALSE
#>  $ UnitTesting   : chr "TRUE"
#>  $ info          : chr "SampleDataMatrix"
#>  $ n             : chr "180"
```

## Contributions

This Git repository uses the [Git
Flow](http://nvie.com/posts/a-successful-git-branching-model/) branching
model (the [`git flow`](https://github.com/petervanderdoes/gitflow-avh)
extension is useful for this). The
[`dev`](https://github.com/dleutnant/influxdbr/tree/dev) branch contains
the latest contributions and other code that will appear in the next
release, and the [`master`](https://github.com/dleutnant/influxdbr)
branch contains the code of the latest release, which is exactly what is
currently on [CRAN](https://cran.r-project.org/package=influxdbr).

Contributing to this package is easy. Just send a [pull
request](https://help.github.com/articles/using-pull-requests/). When
you send your PR, make sure `dev` is the destination branch on the
[influxdbr repository](https://github.com/dleutnant/influxdbr). Your PR
should pass `R CMD check --as-cran`, which will also be checked by
<a href="https://travis-ci.org/dleutnant/influxdbr">Travis CI</a> when
the PR is submitted.

## Code of condcut

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.
