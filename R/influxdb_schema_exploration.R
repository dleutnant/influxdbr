#' @title Show databases
#' @description This function is a convenient wrapper for showing all databases
#' by calling \code{influx_query} with the corresponding query.
#' @param con An influx_connection object (s. \code{influx_connection}).
#'
#' @return A character vector containing the database names.
#' @rdname show_databases
#' @export
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
show_databases <- function(con) {

  result <- influx_query(con = con,
                         query = "SHOW DATABASES",
                         return_xts = F)

  result <- result[[1]][[1]]$databases$name

  return(result)

}

#' @title Show measurements
#' @description This function is a convenient wrapper for showing all measurements
#' by calling \code{influx_query} with the corresponding query.
#' Measurements can be filtered by tag key values with the \code{where} clause.
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param db Sets the target database for the query.
#' @param where Apply filter on tag key values.
#'
#' @return A character vector containing the measurement names.
#' @export
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
show_measurements <- function(con, db, where=NULL) {

  query <- ifelse(is.null(where),
                  "SHOW MEASUREMENTS",
                  paste("SHOW MEASUREMENTS WHERE", where))

  result <- influx_query(con = con,
                         db = db,
                         query = query,
                         return_xts = F)

  result <- result[[1]][[1]]$measurements$name

  return(result)

}

#' @title Show series
#' @description This function is a convenient wrapper for showing all series with distinct
#' key-value pairs by calling \code{influx_query} with
#' the corresponding query. Series can be shown for a specific meausurement with
#' the (\code{measurement}) parameter and can also be filtered by tag key values with
#' the \code{where} clause.
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param db Sets the target database for the query.
#' @param measurement Query a specific measurement.
#' @param where Apply filter on tag key values.
#'
#' @return A list of data.frame objects.
#' @export
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
show_series <- function(con, db, measurement=NULL, where=NULL) {

  query <- ifelse(is.null(measurement),
                  "SHOW SERIES",
                  paste("SHOW SERIES FROM", measurement))

  query <- ifelse(is.null(where),
                  query,
                  paste(query, "WHERE", where))

  result <- influx_query(con = con,
                         db = db,
                         query = query,
                         return_xts = F)

  # extract keys
  result <- result[[1]][[1]][[1]]$key

  # do the conversion
  result <- lapply(result, .influxdb_line_protocol_to_array)

  # produce one data.frame
  result <- Reduce(function(x, y) merge(x, y,
                                        all = TRUE),
                   x = result)

  # old version before influxdb 0.11.0
  # result <- lapply(Reduce(c, result[[1]]),
  #                  FUN = function(x) x[ ,!(colnames(x) == "_key")])

  return(result)
}

#' @title Show tag keys
#' @description This function is a convenient wrapper for showing all unique tag keys
#' associated with each measurement by calling
#' \code{influx_query} with the corresponding query.
#' The query can include a measurement (\code{measurement}) and tag key value (\code{where})
#' conditions, so only certain tag keys are shown.
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param db Sets the target database for the query.
#' @param measurement Query a specific measurement.
#'
#' @return A list of character vectors containing tag keys.
#' @export
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
show_tag_keys <- function(con, db, measurement=NULL) {

  query <- ifelse(is.null(measurement),
                  "SHOW TAG KEYS",
                  paste("SHOW TAG KEYS FROM", measurement))

  result <- influx_query(con = con,
                         db = db,
                         query = query,
                         return_xts = F)

  result <- lapply(Reduce(c, result[[1]]), function(x) as.character(t(x)))

  return(result)
}

#' @title Show tag values
#' @description This function is a convenient wrapper for showing the unique set of
#' tag values for each measurement, for a given tag key by calling
#' \code{influx_query} with the corresponding query.
#' Tag values can be filtered by a specific measurement (\code{measurement}).
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param db Sets the target database for the query.
#' @param measurement Query a specific measurement.
#' @param key The key to be queried.
#'
#' @return A character vector containing tag values.
#' @export
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
show_tag_values <- function(con, db, measurement=NULL, key) {

  query <- ifelse(is.null(measurement),
                  "SHOW TAG VALUES",
                  paste("SHOW TAG VALUES FROM", measurement))

  query <- ifelse(is.null(key),
                  query,
                  paste(query, "WITH KEY=", key))

  result <- influx_query(con = con,
                         db = db,
                         query = query,
                         return_xts = F)

  result <- Reduce(c, result[[1]][[1]])

  # new due to upgrade to 0.11.1
  result <- result$value

  return(result)

}

#' @title Show field keys
#' @description This function is a convenient wrapper for showing the
#' field keys across each measurement in the database by calling
#' \code{influx_query} with the corresponding query.
#' A measurement can be specified (\code{measurement}).
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param db Sets the target database for the query.
#' @param measurement Query a specific measurement.
#'
#' @return A list with data.frames containing field keys.
#' @export
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
show_field_keys <- function(con, db, measurement=NULL) {

  query <- ifelse(is.null(measurement),
                  "SHOW FIELD KEYS",
                  paste("SHOW FIELD KEYS FROM", measurement))

  result <- influx_query(con = con,
                         db = db,
                         query = query,
                         return_xts = F)

  result <- Reduce(c, result[[1]])

  return(result)

}

#' @title Show retention policies
#' @description This function is a convenient wrapper for listing the existent retention
#' policies on a given database by calling \code{influx_query} with the
#' corresponding query.
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param db Sets the target database for the query.
#' @return A data.frame containing the retention plocies.
#' @rdname show_retention_policies
#' @export
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
show_retention_policies <- function(con, db) {

  result <- influx_query(con = con,
                         query = paste("SHOW RETENTION POLICIES ON", db),
                         return_xts = F)

  result <- data.frame(result[[1]])

  return(result)

}
