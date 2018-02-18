#' method send the httr::GET command
#' function is not exported
#' @param con influx_connection object
#' @param endpoint api endpoint
#' @return httr::GET response
#' @keywords internal
httr_GET <- function(con, query = NULL, endpoint) {
  
  response <- tryCatch(httr::GET(url = "",
                                 scheme = con$scheme,
                                 hostname = con$host,
                                 port = con$port,
                                 path = paste0(con$path, endpoint),
                                 query = query,
                                 config = con$config),
                       error = function(e) {print(e); return(NULL)})
  
  return(response)
}


#' method send the httr::POST command
#' function is not exported
#' @param con influx_connection object
#' @param endpoint api endpoint
#' @return httr::POST response
#' @keywords internal
httr_POST <- function(con, query = NULL, body = NULL, endpoint) {
  
  response <- tryCatch(httr::POST(url = "",
                                  body = body,
                                  scheme = con$scheme,
                                  hostname = con$host,
                                  port = con$port,
                                  path = paste0(con$path, endpoint),
                                  query = query,
                                  config = con$config),
                       error = function(e) {print(e); return(NULL)})
  
  return(response)
}

#' method to check the server communication results
#' function is not exported
#' @param x httr::POST response
#' @keywords  internal
check_srv_comm <- function(x) {
  
  # query:
  # HTTP status code	Description
  # 200 OK	Success! The returned JSON offers further information.
  # 400 Bad Request	Unacceptable request. Can occur with a syntactically incorrect 
  #     query. The returned JSON offers further information.
  # 401 Unauthorized	Unacceptable request. Can occur with invalid authentication 
  #     credentials.
  
  # write:
  # HTTP status code	Description
  # 204 No Content	Success!
  # 400 Bad Request	Unacceptable request. Can occur with a Line Protocol syntax 
  #     error or if a user attempts to write values to a field that previously 
  #     accepted a different value type. The returned JSON offers further information.
  # 401 Unauthorized	Unacceptable request. Can occur with invalid authentication 
  #     credentials.
  # 404 Not Found	Unacceptable request. Can occur if a user attempts to write to
  #     a database that does not exist. The returned JSON offers further information.
  # 500 Internal Server Error	The system is overloaded or significantly impaired. 
  #     Can occur if a user attempts to write to a retention policy that does not exist. 
  #     The returned JSON offers further information.
  
  if (!x$status_code %in% c(200, 204)) {
    response_data <- jsonlite::fromJSON(httr::content(x, "text", encoding = "UTF-8"))
    stop(response_data$error, call. = FALSE)
  }
  
  return(NULL)
  
}

#' method to transform precision divisor
#' function is not exported
#' @param x character
#' @keywords  internal
get_precision_divisor <- function(x) {
  div <- switch(
    x,
    "ns" = 1e+9,
    "n" = 1e+9,
    "u" = 1e+6,
    "ms" = 1e+3,
    "s" = 1,
    "m" = 1 / 60,
    "h" = 1 / (60 * 60)
  )
  if(is.null(div)) stop("bad precision argument.")
  return(div)
}
