#' \code{influxdbr} package
#'
#' R Interface for InfluxDB
#'
#' @docType package
#' @name influxdbr
#' @importFrom magrittr %>%
#' @importFrom purrr %||%
NULL

# inspired by https://github.com/jennybc
# quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")  utils::globalVariables(c(".", 
                                                         "tags", 
                                                         "values",
                                                         "info", "value",
                                                         "series_names", 
                                                         "series_tags", 
                                                         "statement_id", 
                                                         "time", 
                                                         "Branch", "uptime", 
                                                         "database", "Alloc",
                                                         "bind"))