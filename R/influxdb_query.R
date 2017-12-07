#' @title Query an InfluxDB server
#' @description This functions queries an InfluxDB server.
#' @param con An `influx_connection` object (s. \code{\link{influx_connection}}).
#' @param db Sets the target database for the query.
#' @param query The InfluxDB query to be sent.
#' @param timestamp_format Sets the timestamp format
#' ("n", "u", "ms", "s", "m", "h").
#' @param return_xts logical. Sets the return type. If set to TRUE, xts objects
#' are returned, FALSE gives tibbles.
#' @param chunked Either FALSE or an integer. If FALSE, series are not requested
#' in streamed batches. If an integer is provided, responses will be chunked by
#' series or by every \code{chunked} points. Chunks are merged internally.
#' @param simplifyList logical. If only one series is returned, the result can 
#' be flatten to directly get either a tibble or an xts object (instead of a list)
#' (default is FALSE).
#'
#' @return A list of tibble or xts objects.
#' @rdname influx_query
#' @export
#' @seealso \code{\link[xts]{xts}}, \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
influx_query <- function(con,
                         db = NULL,
                         query = "SELECT * FROM measurement",
                         timestamp_format = c("n", "u", "ms", "s", "m", "h"),
                         return_xts = TRUE,
                         chunked = FALSE, 
                         simplifyList = FALSE) {
  
  # create query based on function parameters
  q <- list(db = db,
            u = con$user,
            p = con$pass)
  
  # handle different timestamp formats
  timestamp_format <- match.arg(timestamp_format)
  q <- c(q, epoch = timestamp_format)
  
  # handle chunks
  # alternative test: is.wholenumber (s. ?base::integer)
  if (is.numeric(chunked)) {
    q <- c(q,
           chunked = "true",
           chunk_size = ifelse(chunked,
                               10000, # set default to 10000
                               chunked))
  }
  
  # add query
  q <- c(q, q = query)
  
  # submit query
  response <- httr_GET(con = con, query = q, endpoint = "query")
  
  # if curl fails return NULL
  if (is.null(response)) {
    return(NULL)
  }
  
  # Check for communication errors
  check_srv_comm(response)
  
  # debug_data <<- rawToChar(response$content)
  
  # initiate data conversion which result in a tibble with list-columns
  list_of_result <-
    rawToChar(response$content) %>%  # convert to chars
    purrr::map(response_to_list) %>% # from json to list
    purrr::map(query_list_to_tibble, # from list to tibble
               timestamp_format = timestamp_format) %>% 
    purrr::flatten(.)
  
  # xts object required?
  if (return_xts)
    list_of_result <- list_of_result %>%
    purrr::map(tibble_to_xts)
  
  if (simplifyList && (length(list_of_result[[1]]) == 1)) 
    list_of_result <- list_of_result[[1]][[1]]
  
  # if not simplified, a list of results, either a list of tibbles or xts objects 
  # is ALWAYS returned! A wrapping function ALWAYS returns a tibble!
  return(list_of_result)
  
}
