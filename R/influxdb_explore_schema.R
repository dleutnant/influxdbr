#' Show databases
#'
#' This function is a convenient wrapper for showing all databases
#' by calling \code{influx_query} with the corresponding query.
#'
#' @title show_databases
#' @param con An influx_connection object (s. \code{influx_connection}).
#'
#' @return A character vector containing the database names.
#' @rdname show_databases
#' @export
#' @author Dominik Leutnant (\email{leutnant@@fh-muenster.de})
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://influxdb.com/docs/v0.9/query_language/schema_exploration.html}
show_databases <- function(con) {

  result <- influx_query(con = con,
                         query = "SHOW DATABASES",
                         return_xts = F)

  result <- result[[1]]$databases$name

  return(result)

}

#' Show measurements
#'
#' This function is a convenient wrapper for showing all measurements
#' by calling \code{influx_query} with the corresponding query.
#' Measurements can be filtered by tag key values with the \code{where} clause.
#'
#' @title show_measurements
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param db Sets the target database for the query.
#' @param where Apply filter on tag key values.
#'
#' @return A character vector containing the measurement names.
#' @export
#' @author Dominik Leutnant (\email{leutnant@@fh-muenster.de})
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://influxdb.com/docs/v0.9/query_language/schema_exploration.html}
show_measurements <- function(con, db, where=NULL) {

  query <- ifelse(is.null(where),
                  "SHOW MEASUREMENTS",
                  paste("SHOW MEASUREMENTS WHERE", where))

  result <- influx_query(con = con,
                         db = db,
                         query = query,
                         return_xts = F)

  result <- result[[1]]$measurements$name

  return(result)

}

#' Show series
#'
#' This function is a convenient wrapper for showing all series with distinct
#' key-value pairs by calling \code{influx_query} with
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
                  "SHOW SERIES",
                  paste("SHOW SERIES FROM", from))

  query <- ifelse(is.null(where),
                  query,
                  paste(query, "WHERE", where))

  result <- influx_query(con = con,
                         db = db,
                         query = query,
                         return_xts = F)

  result <- lapply(Reduce(c, result),
                   FUN = function(x) x[ ,!(colnames(x) == "_key")])

  return(result)
}

#' Show tag keys
#'
#' This function is a convenient wrapper for showing all unique tag keys
#' associated with each measurement by calling
#' \code{influx_query} with the corresponding query.
#' The query can include a measurement (\code{from}) and tag key value (\code{where})
#' conditions, so only certain tag keys are shown.
#'
#' @title show_tag_keys
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param db Sets the target database for the query.
#' @param from Query a specific measurement.
#'
#' @return A list of character vectors containing tag keys.
#' @export
#' @author Dominik Leutnant (\email{leutnant@@fh-muenster.de})
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://influxdb.com/docs/v0.9/query_language/schema_exploration.html}
show_tag_keys <- function(con, db, from=NULL) {

  query <- ifelse(is.null(from),
                  "SHOW TAG KEYS",
                  paste("SHOW TAG KEYS FROM", from))

  result <- influx_query(con = con,
                         db = db,
                         query = query,
                         return_xts = F)

  result <- lapply(Reduce(c, result), function(x) as.character(t(x)))

  return(result)
}

#' Show tag values
#'
#' This function is a convenient wrapper for showing the unique set of
#' tag values for each measurement, for a given tag key by calling
#' \code{influx_query} with the corresponding query.
#' Tag values can be filtered by a specific measurement (\code{from}).
#'
#' @title show_tag_values
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param db Sets the target database for the query.
#' @param from Query a specific measurement.
#' @param key The key to be queried.
#'
#' @return A character vector containing tag values.
#' @export
#' @author Dominik Leutnant (\email{leutnant@@fh-muenster.de})
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://influxdb.com/docs/v0.9/query_language/schema_exploration.html}
show_tag_values <- function(con, db, from=NULL, key) {

  query <- ifelse(is.null(from),
                  "SHOW TAG VALUES",
                  paste("SHOW TAG VALUES FROM", from))

  query <- ifelse(is.null(key),
                  query,
                  paste(query, "WITH KEY=", key))

  result <- influx_query(con = con,
                         db = db,
                         query = query,
                         return_xts = F)

  result <- Reduce(c, result[[1]][[1]])

  return(result)

}
