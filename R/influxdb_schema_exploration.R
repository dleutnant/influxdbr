#' Influx schema exploration
#'
#' The folllowing functions are convenient wrappers around `influx_query`.
#' * `show_databases()`: returns database names
#' * `show_measurements()`: returns measurement names
#' * `show_series()`: returns unambiguous series
#' * `show_tag_keys()`: returns tag keys
#' * `show_tag_values()`: returns tag values
#' * `show_field_keys()`: returns field keys
#' * `show_retentions_policies()`: returns retention policies
#'
#' @inheritParams influx_query
#' @param measurement Query a specific measurement.
#' @param key The key to be queried.
#' @param where Apply filter on tag key values.
#' @return A tibble containing query results.
#' @name show_databases
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
NULL

#' @export
#' @rdname show_databases
show_databases <- function(con) {
  influx_query(con = con, query = "SHOW DATABASES")
}

#' @export
#' @rdname show_databases
show_measurements <- function(con, db, where = NULL) {
  query <- qpaste("SHOW MEASUREMENTS",
                  "WHERE" = where)
  influx_query(con = con, db = db, query = query)
}

#' @export
#' @rdname show_databases
show_series <- function(con, db, measurement = NULL, where = NULL) {
  query <- qpaste("SHOW SERIES",
                  "FROM" = measurement,
                  "WHERE" = where)
  influx_query(con = con, db = db, query = query)
}

#' @export
#' @rdname show_databases
show_tag_keys <- function(con, db, measurement = NULL) {
  query <- qpaste("SHOW TAG KEYS",
                  "FROM" = measurement)
  influx_query(con = con, db = db, query = query)
}

#' @export
#' @rdname show_databases
show_tag_values <- function(con, db, measurement = NULL, key) {
  oldopts <- options("useFancyQuotes" = FALSE)
  on.exit(options(oldopts))
  query <- qpaste("SHOW TAG VALUES",
                  "FROM" = measurement,
                  "WITH KEY=" = base::dQuote(key))
  influx_query(con = con, db = db, query = query)
}

#' @export
#' @rdname show_databases
show_field_keys <- function(con, db, measurement = NULL) {
  query <- qpaste("SHOW FIELD KEYS",
                  "FROM" = measurement)
  influx_query(con = con, db = db, query = query)
}

#' @export
#' @rdname show_databases
show_retention_policies <- function(con, db) {
  query <- paste("SHOW RETENTION POLICIES ON", db)
  influx_query(con = con, query = query)
}
