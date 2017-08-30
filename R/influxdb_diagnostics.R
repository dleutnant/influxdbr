#' @title Show stats
#' @description This function calls `influx_query` to receive some stats.
#' @inheritParams influx_query
#'
#' @return A tibble.
#' @rdname show_stats
#' @export
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @section Warning:
#' InfluxDB response might take some time.
show_stats <- function(con) {
  result <- influx_query(con = con,
                         query = "SHOW STATS",
                         return_xts = FALSE) %>%
    purrr::map_df( ~ tidyr::unnest(., series_tags, .drop = FALSE)) %>%
    dplyr::select(., series_names, database, Alloc:bind) %>%
    tidyr::gather(info, value, Alloc:bind) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(series_names, database) %>%
    {
      dplyr::mutate(dplyr::ungroup(.), grp_idx = dplyr::group_indices(.))
    } %>%
    split(., .$grp_idx) %>%
    purrr::map_df( ~ dplyr::select(.,-grp_idx) %>%
                     tidyr::nest(info:value))
  
  # TODO: maybe a bit easier?
  
  return(result)
  
}

#' @title Show diagnostics
#' @description This function calls `influx_query` to receive some diagnostics.
#' @inheritParams influx_query
#' @return A tibble with diagnostics.
#' @rdname show_diagnostics
#' @export
#' @seealso \code{\link[influxdbr]{influx_connection}}
show_diagnostics <- function(con) {
  result <- influx_query(con = con,
                         query = "SHOW DIAGNOSTICS",
                         return_xts = FALSE) %>%
    purrr::map_df( ~ dplyr::select(., series_names, Branch:uptime)) %>%
    tidyr::gather(info, value, Branch:uptime) %>%
    dplyr::filter(!is.na(value))
  
  return(result)
}
