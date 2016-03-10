#' Create database
#'
#' This function is a convenient wrapper for creating a database
#' by calling \code{influx_query} with the corresponding query.
#'
#' @title create_database
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param db Sets the target database to create.
#'
#' @return A list of server responses.
#' @rdname create_database
#' @export
#' @author Dominik Leutnant (\email{leutnant@@fh-muenster.de})
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/v0.10/query_language/database_management/}
create_database <- function(con, db) {

  result <- influx_query(con = con,
                         db = db,
                         query = paste("CREATE DATABASE", db),
                         return_xts = FALSE)

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
#' @references \url{https://docs.influxdata.com/influxdb/v0.10/query_language/database_management/}
drop_database <- function(con, db) {

  result <- influx_query(con = con,
                         db = db,
                         query = paste("DROP DATABASE", db),
                         return_xts = FALSE)
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
#' @references \url{https://docs.influxdata.com/influxdb/v0.10/query_language/database_management/}
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
#' @references \url{https://docs.influxdata.com/influxdb/v0.10/query_language/database_management/}
drop_measurement <- function(con, db, measurement) {

  result <- influx_query(con = con,
                         db = db,
                         query = paste("DROP MEASUREMENT",
                         measurement),
                         return_xts = F)
  invisible(result)

}

#' Create retention policy
#'
#' This function is a convenient wrapper for creating a retention policy on a
#' given database by calling \code{influx_query} with the corresponding query.
#'
#' @title create_retention_policy
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param rp_name The name of the retention policy.
#' @param db The name of the database.
#' @param duration Determines how long InfluxDB keeps the data.
#' @param replication The number of data nodes.
#' @param default logical. If TRUE, the new retention policy is the default retention policy
#' for the database.
#' @return A list of server responses.
#' @rdname create_retention_policy
#' @export
#' @author Dominik Leutnant (\email{leutnant@@fh-muenster.de})
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/v0.10/query_language/database_management/}
create_retention_policy <- function(con,
                                    rp_name,
                                    db,
                                    duration,
                                    replication,
                                    default=FALSE) {

  query <- paste("CREATE RETENTION POLICY", rp_name,
                 "ON", db,
                 "DURATION", duration,
                 "REPLICATION", replication)

  if (default) {

    query <- paste(query, "DEFAULT")

  }

  result <- influx_query(con = con,
                         query = query,
                         return_xts = FALSE)

  invisible(result)

}

#' Alter retention policy
#'
#' This function is a convenient wrapper for modifying a retention policy on a
#' given database by calling \code{influx_query} with the corresponding query.
#'
#' @title alter_retention_policy
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param rp_name The name of the retention policy.
#' @param db The name of the database.
#' @param duration Determines how long InfluxDB keeps the data.
#' @param replication The number of data nodes.
#' @param default logical. If TRUE, the new retention policy is the default retention policy
#' for the database.
#' @return A list of server responses.
#' @rdname alter_retention_policy
#' @export
#' @author Dominik Leutnant (\email{leutnant@@fh-muenster.de})
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/v0.10/query_language/database_management/}
alter_retention_policy <- function(con,
                                   rp_name,
                                   db,
                                   duration,
                                   replication,
                                   default = FALSE) {

  query <- paste("ALTER RETENTION POLICY", rp_name,
                 "ON", db,
                 "DURATION", duration,
                 "REPLICATION", replication)

  if (default) {
    query <- paste(query, "DEFAULT")
  }

  result <- influx_query(con = con,
                         query = query,
                         return_xts = FALSE)

  invisible(result)

}

#' Delete retention policy
#'
#' This function is a convenient wrapper for deleting a retention policy on a
#' given database by calling \code{influx_query} with the corresponding query.
#'
#' @title drop_retention_policy
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param rp_name The name of the retention policy.
#' @param db The name of the database.
#' @return A list of server responses.
#' @rdname drop_retention_policy
#' @export
#' @author Dominik Leutnant (\email{leutnant@@fh-muenster.de})
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/v0.10/query_language/database_management/}
drop_retention_policy <- function(con, rp_name, db) {

  query <- paste("DROP RETENTION POLICY", rp_name,
                 "ON", db)

  result <- influx_query(con = con,
                         query = query,
                         return_xts = FALSE)

  invisible(result)

}
