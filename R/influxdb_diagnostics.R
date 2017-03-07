#' @title Show stats
#' @description This function is a convenient wrapper for showing stats
#' by calling \code{influx_query} with the corresponding query.
#' @param con An influx_connection object (s. \code{influx_connection}).
#'
#' @return A list of data.frame objects.
#' @rdname show_stats
#' @export
#' @seealso \code{\link[influxdbr]{influx_connection}}
show_stats <- function(con) {

  result <- influx_query(con = con,
                         query = "SHOW STATS",
                         return_xts = F)

  result <- result[[1]]

  # extract names as identifier
  list_of_names <- sapply(result, names)

  return <- sapply(unique(list_of_names),
                   function(x) Reduce(rbind,
                                      result[which(list_of_names == x)]))

  return(return)

}

#' @title Show diagnostics
#' @description This function is a convenient wrapper for showing diagnostics
#' by calling \code{influx_query} with the corresponding query.
#' @param con An influx_connection object (s. \code{influx_connection}).
#'
#' @return A list with diagnostics.
#' @rdname show_diagnostics
#' @export
#' @seealso \code{\link[influxdbr]{influx_connection}}
show_diagnostics <- function(con) {

  result <- influx_query(con = con,
                         query = "SHOW DIAGNOSTICS",
                         return_xts = F)

  result <- Reduce(c, result[[1]])

  return(result)
}

