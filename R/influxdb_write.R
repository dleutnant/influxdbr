split_ixes <- function(nrows, max_points) {
  # split xts object into a list of xts objects to reduce batch size
  split(1:nrows, rep(1:ceiling((nrows / max_points)), each = max_points)[1:nrows])
}

write_batched <- function(x, con, query, measurement = NULL, measurement_col = NULL,
                          time_col = NULL, tag_cols = NULL, use_integers = FALSE,
                          max_points = 5000, batch_processor = identity) {
  
  progress_bar <- getOption("influxdbr.progress_bar", TRUE)
  
  # split xts object into a list of xts objects to reduce batch size
  ixes <- split_ixes(nrow(x), max_points)

  response <- list()

  do_pb <- progress_bar && length(ixes) > 1

  if (do_pb) {
    pb <- progress::progress_bar$new(format = " Writing [:bar] :percent :elapsed eta: :eta",
      total = length(ixes), width = 100, clear = FALSE)

    pb$tick(0)
  }

  for (i in seq_along(ixes)) {
    resp <-
      x[ixes[[i]], ] %>%
      batch_processor() %>%
      convert_to_line_protocol(measurement = measurement,
        measurement_col = measurement_col,
        time_col = time_col,
        tag_cols = tag_cols,
        precision = query$precision,
        use_integers = use_integers) %>%
      httr_POST(con = con, query = query, body = ., endpoint = "write") %>%
      check_srv_comm(.)

    response[[length(response) + 1]] <- resp

    if (do_pb)
      pb$tick()
  }

  invisible(response)
}

#' @title Write an xts or data.frame object to an InfluxDB server
#'
#' @description This function writes either an `xts` object or a `data.frame` to an InfluxDB server.
#' In case of an xts object, columnnames of the `xts` object are used as InfluxDB's field keys,
#' `xts`'s coredata represent field values. Attributes are preserved and written
#' as tag keys and values, respectively.
#'
#' In case of a `data.frame`, columns may represent times and both tag and field values.
#' Columnnames of the `data.frame` object are used as InfluxDB's tag and field keys.
#' Times and tags are optional. Use parameter `time_col` and `tag_col` to define
#' the interpretation. By specifiying one of the arguments `measurement` or `measurement_col`,
#' a data.frame may contain data from one measurement or multiple measurements, respectively.
#'
#' @inheritParams influx_query
#' @param x The object to write to an InfluxDB server (either of class `xts` or
#'   `data.frame`).
#' @param measurement Sets the name of the measurement (data.frame has data to
#'   write to one measurement only). If both arguments `measurement` and
#'   `measurement_col` are given, `measurement` gets overridden.
#' @param rp Sets the target retention policy for the write. If not present the
#'   default retention policy is used.
#' @param precision Sets the precision of the supplied Unix time values ("s",
#'   "ns", "u", "ms", "m", "h"). If not present timestamps are assumed to be in
#'   seconds.
#' @param consistency Set the number of nodes that must confirm the write. If
#'   the requirement is not met the return value will be partial write if some
#'   points in the batch fail, or write failure if all points in the batch fail.
#' @param max_points Defines the maximum points per batch (defaults to 5000).
#' @param use_integers Should integers (instead of doubles) be written if
#'   present?
#' @param ... Arguments to be passed to methods.
#' @param time_col A character scalar naming the time index column.
#' @param tag_cols A character vector naming tag columns.
#' @param measurement_col A character scalar naming the measurement column
#'   (data.frame has data to write to multiple measurements). Overrides
#'   `measurement` argument.
#' @return A list of server responses.
#' @name influx_write
#' @export
#' @seealso \code{\link[xts]{xts}}, \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
influx_write <- function(x,
                         con,
                         db,
                         measurement,
                         rp = NULL,
                         precision = c("s", "ns", "u", "ms", "m", "h"),
                         consistency = c(NULL, "one", "quroum", "all", "any"),
                         max_points = 5000,
                         use_integers = FALSE,
                         ...) {

  UseMethod("influx_write", x)
}

#' @export
#' @rdname influx_write
influx_write.xts <- function(x,
                             con,
                             db,
                             measurement,
                             rp = NULL,
                             precision = c("s", "ns", "u", "ms", "m", "h"),
                             consistency = c(NULL, "one", "quroum", "all", "any"),
                             max_points = 5000,
                             use_integers = FALSE,
                             ...) {
  
  # create query based on function parameters
  q <- list(db = db,
            u = con$user,
            p = con$pass,
            rp = rp,
            precision = match.arg(precision),
            consistency = match.arg(consistency))

  write_batched(x,
    con = con,
    query = q,
    measurement = measurement,
    max_points = max_points,
    batch_processor = function(xi) {
      xts::xtsAttributes(xi) <- xts::xtsAttributes(x)
      xi
    }
  )
}

#' @export
#' @rdname influx_write
influx_write.data.frame <- function(x,
                                    con,
                                    db,
                                    measurement = NULL,
                                    rp = NULL,
                                    precision = c("s", "ns", "u", "ms", "m", "h"),
                                    consistency = c(NULL, "one", "quroum", "all", "any"),
                                    max_points = 5000,
                                    use_integers = FALSE,
                                    time_col = NULL,
                                    tag_cols = NULL,
                                    measurement_col = NULL,
                                    ...) {
  
  # create query based on function parameters
  q <- list(db = db,
            u = con$user,
            p = con$pass,
            precision = match.arg(precision),
            rp = rp,
            consistency = match.arg(consistency))

  write_batched(x,
    con = con,
    query = q,
    measurement = measurement,
    measurement_col = measurement_col,
    time_col = time_col,
    tag_cols = tag_cols,
    use_integers = use_integers,
    max_points = max_points)

}
