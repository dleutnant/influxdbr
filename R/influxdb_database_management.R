#' Influx database management
#'
#' The folllowing functions are convenient wrappers around `influx_post`.
#' * `create_database()`: creates a new database
#' * `drop_database()`: drops an existing database
#' * `drop_series()`: drops specific series
#' * `delete()`: deletes all points from a series in a database (supports time intervals)
#' * `drop_measurement()`: drops an entire measurement
#' * `create_retention_policy()`: create a new retention policy
#' * `alter_retention_policy()`: alter a retention policy
#' * `drop_retention_policy()`: drop a retention policy
#'
#' @inheritParams influx_query
#' @param measurement Sets a specific measurement.
#' @param where Apply filter on tag key values.
#' @param rp_name The name of the retention policy.
#' @param duration Determines how long InfluxDB keeps the data.
#' @param replication The number of data nodes.
#' @param default logical. If TRUE, the new retention policy is the default retention policy
#' for the database.
#'
#' @return Invisibly a list of class "influxdbr.post.response".
#' @name database_management
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
NULL


#' @export
#' @rdname database_management
create_database <- function(con, db) {
  invisible(influx_post(con = con, query = paste("CREATE DATABASE", db)))
}

#' @export
#' @rdname database_management
drop_database <- function(con, db) {
  invisible(influx_post(con = con,
                        query = paste("DROP DATABASE", db)))
}

#' @export
#' @rdname database_management
drop_series <- function(con,
                        db,
                        measurement = NULL,
                        where = NULL) {
  query <- sprintf("DROP SERIES %s %s",
                   if(is.null(measurement)) "" else paste(query, "FROM", measurement),
                   if(is.null(where)) "" else paste(query, "WHERE", where))
  invisible(influx_post(con = con, db = db, query = query))
}

#' @export
#' @rdname database_management
delete <- function(con,
                   db,
                   measurement = NULL,
                   where = NULL) {
  query <- sprintf("DELETE %s %s",
                   if(is.null(measurement)) "" else paste(query, "FROM", measurement),
                   if(is.null(where)) "" else paste(query, "WHERE", where))
  invisible(influx_post(con = con, db = db, query = query))
}

#' @export
#' @rdname database_management
drop_measurement <- function(con, db, measurement) {
  query <- paste("DROP MEASUREMENT", measurement)
  invisible(influx_post(con = con, db = db, query = query))
}

#' @export
#' @rdname database_management
create_retention_policy <- function(con, db, rp_name, duration,
                                    replication, default = FALSE) {
  query <- paste("CREATE RETENTION POLICY", rp_name,
                 "ON", db,
                 "DURATION", duration,
                 "REPLICATION", replication,
                 if (default) "DEFAULT")
  invisible(influx_post(con = con, db = db, query = query))
}

#' @export
#' @rdname database_management
alter_retention_policy <- function(con,
                                   db,
                                   rp_name,
                                   duration,
                                   replication,
                                   default = FALSE) {
  query <- paste("ALTER RETENTION POLICY", rp_name,
                 "ON", db,
                 "DURATION", duration,
                 "REPLICATION", replication,
                 if (default) "DEFAULT")
  invisible(influx_post(con = con, db = db, query = query))
}

#' @export
#' @rdname database_management
drop_retention_policy <- function(con, db, rp_name) {
  query <- paste("DROP RETENTION POLICY", rp_name, "ON", db)
  invisible(influx_post(con = con, db = db, query = query))
}
