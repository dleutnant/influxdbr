#' @title Create user
#' @description This function is a convenient wrapper for creating a new user
#' by calling \code{influx_query} with the corresponding query.
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param username Sets the username.
#' @param password Sets the password.
#' @rdname create_user
#' @export
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
create_user <- function(con, username, password) {

  options("useFancyQuotes" = FALSE)

  result <- influx_query(con = con,
                         query = paste("CREATE USER", username,
                                       "WITH PASSWORD", base::sQuote(password)),
                         return_xts = F)

  result <- result[[1]]

  return(result)

}

#' @title Drop user
#' @description This function is a convenient wrapper for dropping a user from an influxdb
#' server by calling \code{influx_query} with the corresponding query.
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param username Specifies the user to be dropped.
#' @rdname drop_user
#' @export
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
drop_user <- function(con, username) {

  result <- influx_query(con = con,
                         query = paste("DROP USER", username),
                         return_xts = F)

  result <- result[[1]]

  return(result)

}

#' @title Grant database privileges
#' @description This function is a convenient wrapper for granting database privileges to an
#' existing user by calling \code{influx_query} with the corresponding query.
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param privilege Specifies the user privilege.
#' @param db Sets the target database.
#' @param username Specifies the user.
#' @rdname grant_privileges
#' @export
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
grant_privileges <- function(con, privilege = c("READ", "WRITE", "ALL"), db, username) {

  result <- influx_query(con = con,
                         query = paste("GRANT", match.arg(privilege),
                                       "ON", db,
                                       "TO", username),
                         return_xts = F)

  result <- result[[1]]

  return(result)

}

#' @title Revoke database privileges
#' @description This function is a convenient wrapper for revoking database privileges to an
#' existing user by calling \code{influx_query} with the corresponding query.
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param privilege Specifies the user privilege.
#' @param db Sets the target database.
#' @param username Specifies the user.
#' @rdname revoke_privileges
#' @export
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
revoke_privileges <- function(con, privilege = c("READ", "WRITE", "ALL"), db, username) {

  result <- influx_query(con = con,
                         query = paste("REVOKE", match.arg(privilege),
                                       "ON", db,
                                       "FROM", username),
                         return_xts = F)

  result <- result[[1]]

  return(result)

}

#' @title Show users
#' @description This function is a convenient wrapper for listing all existent users
#' by calling \code{influx_query} with the corresponding query.
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @rdname show_users
#' @export
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
show_users <- function(con) {

  result <- influx_query(con = con,
                         query = "SHOW USERS",
                         return_xts = F)

  result <- data.frame(result[[1]])

  return(result)

}

#' @title Show grants
#' @description This function is a convenient wrapper for listing a user's database privileges
#' by calling \code{influx_query} with the corresponding query.
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param user The username.
#' @rdname show_grants
#' @export
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
show_grants <- function(con, user) {

  result <- influx_query(con = con,
                         query = paste("SHOW GRANTS FOR", user),
                         return_xts = F)

  result <- data.frame(result[[1]])

  return(result)

}
