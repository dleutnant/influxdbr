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
  result <- influx_query(con = con,
                         query = "SHOW DATABASES",
                         return_xts = FALSE) %>%
    purrr::map_df( ~ dplyr::select(., name))
  
  return(result)
  
}

#' @export
#' @rdname show_databases
show_measurements <- function(con, db, where = NULL) {
  query <- ifelse(is.null(where),
                  "SHOW MEASUREMENTS",
                  paste("SHOW MEASUREMENTS WHERE", where))
  
  result <- influx_query(
    con = con,
    db = db,
    query = query,
    return_xts = FALSE
  ) %>%
    purrr::map_df( ~ dplyr::select(., name))
  
  return(result)
  
}

#' @export
#' @rdname show_databases
show_series <- function(con,
                        db,
                        measurement = NULL,
                        where = NULL) {
  query <- ifelse(is.null(measurement),
                  "SHOW SERIES",
                  paste("SHOW SERIES FROM", measurement))
  
  query <- ifelse(is.null(where),
                  query,
                  paste(query, "WHERE", where))
  
  result <- influx_query(
    con = con,
    db = db,
    query = query,
    return_xts = FALSE
  ) %>%
    purrr::map_df( ~ dplyr::select(., key)) %>%
    .[["key"]] %>%
    purrr::map_df(line_protocol_to_array) %>%
    tibble::as_tibble()
  
  return(result)
}

#' @export
#' @rdname show_databases
show_tag_keys <- function(con, db, measurement = NULL) {
  query <- ifelse(is.null(measurement),
                  "SHOW TAG KEYS",
                  paste("SHOW TAG KEYS FROM", measurement))
  
  result <- influx_query(
    con = con,
    db = db,
    query = query,
    return_xts = FALSE
  ) %>%
    purrr::map_df( ~ dplyr::select(., series_names, tagKey))
  
  return(result)
}

#' @export
#' @rdname show_databases
show_tag_values <- function(con, db, measurement = NULL, key) {
  
  # check Option useFancyQuotes
  quotes <- getOption("useFancyQuotes")
  on.exit(options("useFancyQuotes" = quotes))
  options("useFancyQuotes" = FALSE)
  
  query <- ifelse(is.null(measurement),
                  "SHOW TAG VALUES",
                  paste("SHOW TAG VALUES FROM", measurement)) %>%
    paste(., " WITH KEY=", base::dQuote(key), sep = "")
  
  result <- influx_query(
    con = con,
    db = db,
    query = query,
    return_xts = FALSE
  ) %>%
    purrr::map_df( ~ dplyr::select(., series_names, key, value))
  
  return(result)
  
}

#' @export
#' @rdname show_databases
show_field_keys <- function(con, db, measurement = NULL) {
  query <- ifelse(is.null(measurement),
                  "SHOW FIELD KEYS",
                  paste("SHOW FIELD KEYS FROM", measurement))
  
  result <- influx_query(
    con = con,
    db = db,
    query = query,
    return_xts = FALSE
  ) %>%
    purrr::map_df( ~ dplyr::select(., series_names, fieldKey, fieldType))
  
  return(result)
  
}

#' @export
#' @rdname show_databases
show_retention_policies <- function(con, db) {
  result <- influx_query(
    con = con,
    query = paste("SHOW RETENTION POLICIES ON", db),
    return_xts = FALSE
  ) %>%
    purrr::map_df( ~ dplyr::select(., name,
                                   duration,
                                   shardGroupDuration,
                                   replicaN,
                                   default))
  
  return(result)
  
}
