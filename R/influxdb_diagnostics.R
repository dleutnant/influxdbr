#' @title Show diagnostics and stats.
#' @description These functions calls `influx_query` to receive some stats.
#' @inheritParams influx_query
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @section Warning: \code{show_stats} might take some time.
#' @return A data.frame frame.
#' @rdname diagnostics
#' @export
show_stats <- function(con) {

  influx_query(con = con, query = "SHOW STATS",
               handler = identity)[[1]][[1]]$series %>%
    flatten_series("STAT")
}

#' @rdname diagnostics
#' @export
show_diagnostics <- function(con) {
  influx_query(con = con,
               query = "SHOW DIAGNOSTICS",
               handler = identity)[[1]][[1]]$series %>%
    flatten_series("DIAGNOSTIC")
}

flatten_series <- function(series, name) {
  do.call(rbind, 
          lapply(series, function(s) {
            data.frame(category = s$name,
                       measure = unlist(s$columns),
                       value = unlist(s$values))
          })) %>%
    stats::setNames(c("CATEGORY", name, "VALUE"))
}
