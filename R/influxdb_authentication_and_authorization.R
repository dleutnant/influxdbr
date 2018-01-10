#' Influx authentication and authorization
#'
#' The folllowing functions are convenient wrappers around `influx_post`
#' and `influx_query` (show_users and show grants).
#' * `create_user()`: creates a new user
#' * `drop_user()`: drops an existing user
#' * `grant_privileges()`: grant privileges to an existing users
#' * `revoke_privileges()`: revoke privileges to an existing users
#' * `show_users()`: show all users
#' * `show_grants()`: show grants of an user
#'
#' @inheritParams influx_post
#' @param username The username to be used.
#' @param password The password to be used.
#' @param privilege Specifies the user privilege.
#' @param db  Sets the target database.
#' @return A tibble containing post results in case of an error (or message).
#' Otherwise NULL (invisibly). `show_users()` and `show_grants()` return a tibble.
#' @name create_user
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
NULL

#' @export
#' @rdname create_user
create_user <- function(con, username, password) {
  options("useFancyQuotes" = FALSE)
  
  result <- influx_post(
    con = con,
    query = paste(
      "CREATE USER",
      username,
      "WITH PASSWORD",
      base::sQuote(password)
    )
  )
  
  
  if (!is.null(result)) {
    return(result)
  }
  
  invisible(result)
  
}

#' @export
#' @rdname create_user
drop_user <- function(con, username) {
  result <- influx_post(con = con,
                        query = paste("DROP USER", username))
  
  if (!is.null(result)) {
    return(result)
  }
  
  invisible(result)
  
}

#' @export
#' @rdname create_user
grant_privileges <- function(con,
                             privilege = c("READ", "WRITE", "ALL"),
                             db,
                             username) {
  result <- influx_post(con = con,
                        query = paste("GRANT", match.arg(privilege),
                                      "ON", db,
                                      "TO", username))
  
  if (!is.null(result)) {
    return(result)
  }
  
  invisible(result)
  
}

#' @export
#' @rdname create_user
revoke_privileges <- function(con,
                              privilege = c("READ", "WRITE", "ALL"),
                              db,
                              username) {
  result <- influx_post(con = con,
                        query = paste("REVOKE", match.arg(privilege),
                                      "ON", db,
                                      "FROM", username))
  
  if (!is.null(result)) {
    return(result)
  }
  
  invisible(result)
  
}

#' @export
#' @rdname create_user
show_users <- function(con) {
  result <- influx_query(con = con,
                         query = "SHOW USERS",
                         return_xts = FALSE) %>%
    purrr::map_df( ~ dplyr::select(., user, admin))
  
  return(result)
  
}

#' @export
#' @rdname create_user
show_grants <- function(con, username) {
  result <- influx_query(
    con = con,
    query = paste("SHOW GRANTS FOR",
                  username),
    return_xts = FALSE
  ) %>%
    purrr::map_df( ~ dplyr::select(., database, privilege))
  
  return(result)
  
}
