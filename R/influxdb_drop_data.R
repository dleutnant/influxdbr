#' Drop measurement
#'
#' This function is a convenient wrapper for dropping an entire measurement
#' by calling \code{influx_query} with the corresponding query.
#'
#' @title drop_measurement
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param db Sets the target database for the query.
#' @param measurement Sets the measurement to be dropped.
#'
#' @return A list of server responses.
#' @rdname drop_measurement
#' @export
#' @author Dominik Leutnant (\email{leutnant@@fh-muenster.de})
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://influxdb.com/docs/v0.9/query_language/data_exploration.html}
drop_measurement <- function(con, db, measurement) {

  result <- influx_query(con = con,
                         db = db,
                         query = paste("DROP MEASUREMENT",
                         measurement),
                         return_xts = F)
  invisible(result)

}

#' Drop series
#'
#' This function is a convenient wrapper for dropping an individual series
#' within a measurement that match given tags by calling \code{influx_query}
#' with the corresponding query.
#'
#' @title drop_series
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param db Sets the target database for the query.
#' @param id Sets the series ID.
#' @param from Sets a specific measurement.
#' @param where Apply filter on tag key values.
#'
#' @return A list of server responses.
#' @rdname drop_series
#' @export
#' @author Dominik Leutnant (\email{leutnant@@fh-muenster.de})
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://influxdb.com/docs/v0.9/query_language/data_exploration.html}
drop_series <- function(con, db, id=NULL, from=NULL, where=NULL) {

  query <- "DROP SERIES"

  if (!is.null(id)) {

    query <- paste(query, id)

  } else {

    query <- ifelse(is.null(from),
                    query,
                    paste(query, "FROM", from))

    query <- ifelse(is.null(where),
                    query,
                    paste(query, "WHERE", where))

  }

  result <- influx_query(con = con,
                         db = db,
                         query = query,
                         return_xts = F)

  invisible(result)

}

#' Drop databases
#'
#' This function is a convenient wrapper for dropping a database
#' by calling \code{influx_query} with the corresponding query.
#'
#' @title drop_database
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param db Sets the target database to drop.
#'
#' @return A list of server responses.
#' @rdname drop_database
#' @export
#' @author Dominik Leutnant (\email{leutnant@@fh-muenster.de})
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://influxdb.com/docs/v0.9/query_language/data_exploration.html}
drop_database <- function(con, db) {

  result <- influx_query(con = con,
                         db = db,
                         query = paste("DROP DATABASE", db),
                         return_xts = FALSE)
  invisible(result)

}
