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
#' @param verbose logical. Occasionally, an influxdb server might be queried without
#' specifying 'measurement' and/or 'where' which could result in a large number of
#' individual series. Due to the function's purpose to result one single data.frame
#' containing all series, the merging process of mulitple data.frames with unequal
#' colnames potentially take some time. If \code{verbose} is set to \code{TRUE},
#' the user is informed about an evtl. long running process and can interrupt the
#' function call. The server result will be returned unpolished in this case.
#' Defaults to TRUE.
#'
#' @return A list of data.frame objects.
#' @export
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
show_series <- function(con, db, measurement=NULL, where=NULL, verbose = TRUE) {

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

  # a general "show series" might result in a large number of individual series.
  # therefore, before converting to one single data.frame, we kindly inform the user
  # about a pontentially long runnnig conversion (which is mainly caused by REDUCE ...)
  if ((verbose) & (length(result) > 1000)) {
    choice <- select.list(choices = c("yes", "no"),
                          title = paste("Merging multiple data.frames (", length(result),
                                        ") might take a long time. Do you want to continue? Otherwise refine the query with 'measurement' or 'where'.",
                                        sep = ""))
    if (choice != "yes") return(result)
  }

  # do the conversion
  result <- lapply(result, .influxdb_line_protocol_to_array)

  ## TODO: OR MAYBE RETURN A LIST INSTEAD???

  # produce one data.frame
  result <- Reduce(function(x, y) merge(x, y, all = TRUE), x = result)

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
