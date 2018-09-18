# influxdbr 0.14.3.9000

## Bug fixes

* simplifyList argument of `influx_query()` now works as expected

* Encoding Fix, defaults to UTF-8 (@Avsamoht).

* Empty query results yield to `NULL` instead of raising an error (#46).

# influxdbr 0.14.2

## New features

* added IQL wrapper for `delete`

* `influx_connection` gains new argument `curl_options` to control http communication (#36)

* `influx_write` function gets `measurement_col` argument to enable writing a 
data.frame with multiple measurements with only one function call

## Internals

* package dependencies specified for `dplyr` and `purrr` (#21, #38, #39)

* new internal helper functions for `httr::GET` and `httr::POST`
  
# influxdbr 0.14.0

## Breaking changes

* `influx_write` function argument `xts` changes to `x` (method is now generic)

## New features

* `influx_write` now accepts data.frames to upload mulitple tags and fields per timestamp (#20).

## Bug fixes

* correct parsing of json structure without `statement_id` (#32).

* special characters in `xts` attributes are now supported (#30).

* Fix special character representation in `show_series`.

* Using double quotes in string arguments of `influx_query` wrappers (e.g. `influx_select`, `show_tag_values`) (#22).

* IQL wrapper functions with no expected return value now use `influx_post` instead of `influx_query`.  

# influxdbr 0.13.0

* Fix coercion error in `influx_write()` in case of sub-second accuracy (#25).

* `influx_select()` correctly parses integer arguments (#27).

* Significantly improved performance of `influx_query()` (#28).

# influxdbr 0.12.0

* First CRAN release