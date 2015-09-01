#' Show databases
#'
#' This function is a convenient wrapper for showing all databases of an
#' influxdb by calling \code{influx_query} with the corresponding query.
#'
#' @title show_databases
#' @param con An influx_connection object (s. \code{influx_connection}).
#'
#' @return A list of data.frame objects.
#' @rdname show_databases
#' @export
#' @author Dominik Leutnant (\email{leutnant@@fh-muenster.de})
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://influxdb.com/docs/v0.9/query_language/schema_exploration.html}
show_databases <- function(con) {

  result <- influxdbr::influx_query(con = con,
                                    query = "show databases",
                                    return_xts = F)

  return(result)

}

#' Show measurements
#'
#' This function is a convenient wrapper for showing all measurements of an
#' influxdb database by calling \code{influx_query} with the corresponding query.
#' Measurements can be filtered by tag key values with the \code{where} clause.
#'
#' @title show_measurements
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param db Sets the target database for the query.
#' @param where Apply filter on tag key values.
#'
#' @return A list of data.frame objects.
#' @export
#' @author Dominik Leutnant (\email{leutnant@@fh-muenster.de})
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://influxdb.com/docs/v0.9/query_language/schema_exploration.html}
show_measurements <- function(con, db, where=NULL) {

  query <- ifelse(is.null(where),
                  "show measurements",
                  paste("show measurements where", where))

  result <- influxdbr::influx_query(con = con,
                                    db = db,
                                    query = query,
                                    return_xts = F)

  return(result)

}

#' Show series
#'
#' This function is a convenient wrapper for showing all series with distinct
#' key-value pairs of an influxdb database by calling \code{influx_query} with
#' the corresponding query. Series can be shown for a specific meausurement with
#' the (\code{from}) parameter and can also be filtered by tag key values with
#' the \code{where} clause.
#'
#' @title show_series
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param db Sets the target database for the query.
#' @param from Query a specific measurement.
#' @param where Apply filter on tag key values.
#'
#' @return A list of data.frame objects.
#' @export
#' @author Dominik Leutnant (\email{leutnant@@fh-muenster.de})
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://influxdb.com/docs/v0.9/query_language/schema_exploration.html}
show_series <- function(con, db, from=NULL, where=NULL) {

  query <- ifelse(is.null(from),
                  "show series",
                  paste("show series from", from))

  query <- ifelse(is.null(where),
                  query,
                  paste(query, "where", where))

  result <- influxdbr::influx_query(con = con,
                                    db = db,
                                    query = query,
                                    return_xts = F)

  return(result)
}

#' Show tag keys
#'
#' This function is a convenient wrapper for showing all unique tag keys
#' associated  with each measurement of an influxdb database by calling
#' \code{influx_query} with the corresponding query.
#' The query can include a measurement (\code{from}) and tag key value (\code{where})
#' conditions, so only certain tag keys are shown.
#'
#' @title show_series
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param db Sets the target database for the query.
#' @param from Query a specific measurement.
#' @param where Apply filter on tag key values.
#'
#' @return A list of data.frame objects.
#' @export
#' @author Dominik Leutnant (\email{leutnant@@fh-muenster.de})
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://influxdb.com/docs/v0.9/query_language/schema_exploration.html}
show_tag_keys <- function(con, db, from=NULL, where=NULL) {

  query <- ifelse(is.null(from),
                  "show tag keys",
                  paste("show tag keys from", from))

  query <- ifelse(is.null(where),
                  query,
                  paste(query, "where", where))

  result <- influxdbr::influx_query(con = con,
                                    db = db,
                                    query = query,
                                    return_xts = F)

  return(result)
}

#' Show tag values
#'
#' This function is a convenient wrapper for showing the unique set of
#' tag values for each measurement, for a given tag key of an influxdb database
#' by calling \code{influx_query} with the corresponding query.
#' Tag values can be filtered by a specific measurement (\code{from}).
#'
#' @title show_series
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param db Sets the target database for the query.
#' @param from Query a specific measurement.
#' @param where Apply filter on tag key values.
#'
#' @return A list of data.frame objects.
#' @export
#' @author Dominik Leutnant (\email{leutnant@@fh-muenster.de})
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://influxdb.com/docs/v0.9/query_language/schema_exploration.html}
show_tag_values <- function(con, db, from=NULL, key) {

  query <- ifelse(is.null(from),
                  "show tag values",
                  paste("show tag values from", from))

  query <- ifelse(is.null(key),
                  query,
                  paste(query, "with key=", key))

  result <- influxdbr::influx_query(con = con,
                                    db = db,
                                    query = query,
                                    return_xts = F)

  return(result)

}

