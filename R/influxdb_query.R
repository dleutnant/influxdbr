#' @title Query an InfluxDB server
#' @description This functions queries an InfluxDB server.
#' @param con An `influx_connection` object
#'   (s. \code{\link{influx_connection}}).
#' @param db Sets the target database for the query.
#' @param query The InfluxDB query to be sent.
#' @param timestamp_format Sets the timestamp format ("n", "u", "ms", "s", "m",
#'   "h").
#' @param tags_as_factors If TRUE, convert tag columns to factors automatically.
#' @param chunked Either FALSE or an integer. If FALSE, series are not requested
#'   in streamed batches. If an integer is provided, responses will be chunked
#'   by series or by every \code{chunked} points. If \code{handler} is not
#'   supplied chunks are aggregated into a data.frame.
#' @param handler A function receiving a list of parsed json objects when
#'   \code{chunked} it not FALSE. \code{influx_query} returns a list of values
#'   returned by \code{handler}. If handler returns NULL the value is omitted
#'   from the output. Supply \code{identity} to retrieve raw parsed json.
#' @param verbose If TRUE print log messages.
#' @return A list a data.frame object. Empty query returns NULL.
#' @rdname influx_query
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
#' @export
influx_query <- function(con,
                         db = NULL,
                         query = "SELECT * FROM measurement",
                         timestamp_format = c("n", "u", "ms", "s", "m", "h"),
                         tags_as_factors = TRUE,
                         chunked = FALSE, 
                         handler = NULL,
                         verbose = FALSE,
                         csv = FALSE) {
  
  q <- list(db = db,
            u = con$user,
            p = con$pass,
            q = query, 
            epoch = match.arg(timestamp_format))
    
  if (is.numeric(chunked)) {
    q[["chunked"]] <- "true"
    q[["chunk_size"]] <- if(chunked == 0) 10000 else chunked
  } else if (chunked) {
    q[["chunked"]] <- "true"
  }

  if (verbose) log("Sending influx query ...")
  response <- httr_GET(con = con, query = q, endpoint = "query", csv = csv)
  
  check_response_errors(response)

  if (csv) {
    if (verbose) log("Parsing csv ...")
    content <- httr::content(response, "text", encoding = "UTF-8")
    ## cat(content)
    out <- parse_csv(content)
  } else {
    if (verbose) log("Parsing json ...")
    out <- parse_response(response$content, chunked, handler)
    ## str(json)
    if(is.null(handler)) {
      if (verbose) log("Binding into a data.frame ...")
      out <- json_to_df(out, match.arg(timestamp_format), tags_as_factors)
    }
  }

  if (verbose) log("Done!")
  out
}

log <- function(...) {
  str <- paste(..., collapse = "", sep = "")
  message(sprintf("[%s] %s", Sys.time(), str))
}
