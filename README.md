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
# install.packages("remotes")
remotes::install_github("dleutnant/influxdbr@dev")
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

# assign some attributes
xts::xtsAttributes(xts_data) <- list(info = "SampleDataMatrix",
                                     UnitTesting = TRUE, 
                                     n = 180,
                                     source = "xts")
                                     
# print structure to inspect the object
str(xts_data)
#> An 'xts' object on 2007-01-02/2007-06-30 containing:
#>   Data: num [1:180, 1:4] 50 50.2 50.4 50.4 50.2 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : NULL
#>   ..$ : chr [1:4] "Open" "High" "Low" "Close"
#>   Indexed by objects of class: [POSIXct,POSIXt] TZ: 
#>   xts Attributes:  
#> List of 4
#>  $ info       : chr "SampleDataMatrix"
#>  $ UnitTesting: logi TRUE
#>  $ n          : num 180
#>  $ source     : chr "xts"
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
#> # A tibble: 10 x 1
#>    name      
#>    <chr>     
#>  1 _internal 
#>  2 stbmod    
#>  3 wasig     
#>  4 wasig-fr  
#>  5 wasig-h   
#>  6 test      
#>  7 oscar_test
#>  8 longterm  
#>  9 deznwba   
#> 10 mydb
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
             measurement = "sampledata")
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
                n = row_number(), # add tag 'n'
                source = "df")  # add source 'df'

df_data
#> # A tibble: 180 x 9
#>    time                 Open  High   Low Close info     UnitT…     n sour…
#>    <dttm>              <dbl> <dbl> <dbl> <dbl> <chr>    <lgl>  <int> <chr>
#>  1 2007-01-02 00:00:00  50.0  50.1  50.0  50.1 SampleD… T          1 df   
#>  2 2007-01-03 00:00:00  50.2  50.4  50.2  50.4 SampleD… T          2 df   
#>  3 2007-01-04 00:00:00  50.4  50.4  50.3  50.3 SampleD… T          3 df   
#>  4 2007-01-05 00:00:00  50.4  50.4  50.2  50.3 SampleD… T          4 df   
#>  5 2007-01-06 00:00:00  50.2  50.2  50.1  50.2 SampleD… T          5 df   
#>  6 2007-01-07 00:00:00  50.1  50.2  50.0  50.0 SampleD… T          6 df   
#>  7 2007-01-08 00:00:00  50.0  50.1  50.0  50.0 SampleD… T          7 df   
#>  8 2007-01-09 00:00:00  50.0  50.0  49.8  49.9 SampleD… T          8 df   
#>  9 2007-01-10 00:00:00  49.9  50.1  49.9  50.0 SampleD… T          9 df   
#> 10 2007-01-11 00:00:00  49.9  50.2  49.9  50.2 SampleD… T         10 df   
#> # ... with 170 more rows

# write example data.frame to database
influx_write(con = con, 
             db = "mydb",
             x = df_data,
             time_col = "time", tag_cols = c("info", "UnitTesting", "n", "source"),
             measurement = "sampledata")
```

We can now check if the time series were succefully written:

``` r
# check if measurements were succefully written
show_measurements(con = con, db = "mydb")
#> # A tibble: 1 x 1
#>   name      
#>   <chr>     
#> 1 sampledata
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
                        where = "source = 'df'",
                        group_by = "*",
                        limit = 10, 
                        order_desc = TRUE, 
                        return_xts = FALSE)

result
#> [[1]]
#> # A tibble: 180 x 10
#>    state… serie… serie… Unit… info   n     sour… time                 Open
#>     <int> <chr>  <lgl>  <chr> <chr>  <chr> <chr> <dttm>              <dbl>
#>  1      0 sampl… F      TRUE  Sampl… 99    df    2007-04-09 22:00:00  49.6
#>  2      0 sampl… F      TRUE  Sampl… 98    df    2007-04-08 22:00:00  49.4
#>  3      0 sampl… F      TRUE  Sampl… 97    df    2007-04-07 22:00:00  49.5
#>  4      0 sampl… F      TRUE  Sampl… 96    df    2007-04-06 22:00:00  49.5
#>  5      0 sampl… F      TRUE  Sampl… 95    df    2007-04-05 22:00:00  49.3
#>  6      0 sampl… F      TRUE  Sampl… 94    df    2007-04-04 22:00:00  49.4
#>  7      0 sampl… F      TRUE  Sampl… 93    df    2007-04-03 22:00:00  49.2
#>  8      0 sampl… F      TRUE  Sampl… 92    df    2007-04-02 22:00:00  49.1
#>  9      0 sampl… F      TRUE  Sampl… 91    df    2007-04-01 22:00:00  48.9
#> 10      0 sampl… F      TRUE  Sampl… 90    df    2007-03-31 22:00:00  48.9
#> # ... with 170 more rows, and 1 more variable: High <dbl>
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
                        where = "source = 'xts'",
                        group_by =  "*",
                        limit = 10, 
                        order_desc = TRUE, 
                        return_xts = TRUE)

str(result)
#> List of 1
#>  $ :List of 2
#>   ..$ sampledata:An 'xts' object on 2007-06-20 22:00:00/2007-06-29 22:00:00 containing:
#>   Data: num [1:10, 1] 47.7 47.6 47.2 47.2 47.2 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : NULL
#>   ..$ : chr "Open"
#>   Indexed by objects of class: [POSIXct,POSIXt] TZ: GMT
#>   xts Attributes:  
#> List of 7
#>   .. ..$ statement_id  : int 0
#>   .. ..$ series_names  : chr "sampledata"
#>   .. ..$ series_partial: logi FALSE
#>   .. ..$ UnitTesting   : chr "TRUE"
#>   .. ..$ info          : chr "SampleDataMatrix"
#>   .. ..$ n             : chr "180"
#>   .. ..$ source        : chr "xts"
#>   ..$ sampledata:An 'xts' object on 2007-06-20 22:00:00/2007-06-29 22:00:00 containing:
#>   Data: num [1:10, 1] 47.7 47.6 47.2 47.3 47.4 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : NULL
#>   ..$ : chr "High"
#>   Indexed by objects of class: [POSIXct,POSIXt] TZ: GMT
#>   xts Attributes:  
#> List of 7
#>   .. ..$ statement_id  : int 0
#>   .. ..$ series_names  : chr "sampledata"
#>   .. ..$ series_partial: logi FALSE
#>   .. ..$ UnitTesting   : chr "TRUE"
#>   .. ..$ info          : chr "SampleDataMatrix"
#>   .. ..$ n             : chr "180"
#>   .. ..$ source        : chr "xts"
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
                        where = "source = 'df'",
                        group_by =  "*",
                        limit = 10, 
                        order_desc = TRUE, 
                        return_xts = FALSE, 
                        simplifyList = TRUE)

str(result)
#> List of 1
#>  $ :Classes 'tbl_df', 'tbl' and 'data.frame':    180 obs. of  9 variables:
#>   ..$ statement_id  : int [1:180] 0 0 0 0 0 0 0 0 0 0 ...
#>   ..$ series_names  : chr [1:180] "sampledata" "sampledata" "sampledata" "sampledata" ...
#>   ..$ series_partial: logi [1:180] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>   ..$ UnitTesting   : chr [1:180] "TRUE" "TRUE" "TRUE" "TRUE" ...
#>   ..$ info          : chr [1:180] "SampleDataMatrix" "SampleDataMatrix" "SampleDataMatrix" "SampleDataMatrix" ...
#>   ..$ n             : chr [1:180] "99" "98" "97" "96" ...
#>   ..$ source        : chr [1:180] "df" "df" "df" "df" ...
#>   ..$ time          : POSIXct[1:180], format: "2007-04-09 22:00:00" ...
#>   ..$ Open          : num [1:180] 49.6 49.4 49.5 49.5 49.3 ...
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

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.

## Citation

To cite package ‘influxdbr’ in publications use:

Dominik Leutnant (2018). influxdbr: R Interface to InfluxDB. R package
version 0.14.2. <https://github.com/dleutnant/influxdbr>

A BibTeX entry for LaTeX users is

@Manual{, title = {influxdbr: R Interface to InfluxDB}, author =
{Dominik Leutnant}, year = {2018}, note = {R package version 0.14.2},
url = {<https://github.com/dleutnant/influxdbr>}, }
