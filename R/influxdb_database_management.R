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
#' @return A tibble containing post results in case of an error (or message).
#' Otherwise NULL (invisibly).
#' @name create_database
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
NULL


#' @export
#' @rdname create_database
create_database <- function(con, db) {
  result <- influx_post(con = con,
                        query = paste("CREATE DATABASE", db))
  
  if (!is.null(result)) {
    return(result)
  }
  
  invisible(result)
  
}

#' @export
#' @rdname create_database
drop_database <- function(con, db) {
  result <- influx_post(con = con,
                        query = paste("DROP DATABASE", db))
  
  if (!is.null(result)) {
    return(result)
  }
  
  invisible(result)
}

#' @export
#' @rdname create_database
drop_series <- function(con,
                        db,
                        measurement = NULL,
                        where = NULL) {
  
  query <- "DROP SERIES"
  
  query <- ifelse(is.null(measurement),
                  query,
                  paste(query, "FROM", measurement))
  
  query <- ifelse(is.null(where),
                  query,
                  paste(query, "WHERE", where))
  
  result <- influx_post(
    con = con,
    db = db,
    query = query
  )
  
  if (!is.null(result)) {
    return(result)
  }
  
  invisible(result)
  
}

#' @export
#' @rdname create_database
delete <- function(con,
                   db,
                   measurement = NULL,
                   where = NULL) {
  
  query <- "DELETE"
  
  query <- ifelse(is.null(measurement),
                  query,
                  paste(query, "FROM", measurement))
  
  query <- ifelse(is.null(where),
                  query,
                  paste(query, "WHERE", where))
  
  result <- influx_post(
    con = con,
    db = db,
    query = query
  )
  
  if (!is.null(result)) {
    return(result)
  }
  
  invisible(result)
  
}

#' @export
#' @rdname create_database
drop_measurement <- function(con, db, measurement) {
  result <- influx_post(
    con = con,
    db = db,
    query = paste("DROP MEASUREMENT",
                  measurement)
  )
  
  if (!is.null(result)) {
    return(result)
  }
  
  invisible(result)
  
}

#' @export
#' @rdname create_database
create_retention_policy <- function(con,
                                    db,
                                    rp_name,
                                    duration,
                                    replication,
                                    default = FALSE) {
  query <- paste(
    "CREATE RETENTION POLICY",
    rp_name,
    "ON",
    db,
    "DURATION",
    duration,
    "REPLICATION",
    replication
  )
  
  if (default) {
    query <- paste(query, "DEFAULT")
    
  }
  
  result <- influx_post(con = con,
                        db = db,
                        query = query)
  
  if (!is.null(result)) {
    return(result)
  }
  
  invisible(result)
  
}

#' @export
#' @rdname create_database
alter_retention_policy <- function(con,
                                   db,
                                   rp_name,
                                   duration,
                                   replication,
                                   default = FALSE) {
  query <- paste(
    "ALTER RETENTION POLICY",
    rp_name,
    "ON",
    db,
    "DURATION",
    duration,
    "REPLICATION",
    replication
  )
  
  if (default) {
    query <- paste(query, "DEFAULT")
  }
  
  result <- influx_post(con = con,
                        db = db,
                        query = query)
  
  if (!is.null(result)) {
    return(result)
  }
  
  invisible(result)
  
}

#' @export
#' @rdname create_database
drop_retention_policy <- function(con,
                                  db,
                                  rp_name) {
  query <- paste("DROP RETENTION POLICY", rp_name,
                 "ON", db)
  
  result <- influx_post(con = con,
                        db = db,
                        query = query)
  
  if (!is.null(result)) {
    return(result)
  }
  
  invisible(result)
  
}
