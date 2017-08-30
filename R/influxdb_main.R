#' @title Create an influxdb_connection object
#' @description Create an influxdb_connection object by specifying server
#' connection details. Credentials can also be saved and accessed through a 
#' config file.
#' @param scheme The scheme to use, either http or https. Defaults to http.
#' @param host Hostname of the InfluxDB server. Defaults to localhost
#' @param port numerical. Port number of the InfluxDB server. Defaults to 8086.
#' @param user username The username to use. Defaults to "user"
#' @param pass password The password to use. Defaults to "pass".
#' @param path The prefix path on which the InfluxDB is running. Can be useful 
#' in proxy situations.
#' @param group The group to use within the config file.
#' @param verbose logical. Provide additional details?
#' @param config_file The configuration file to be used if `group` is
#' specified.
#' @rdname influx_connection
#' @export
#' @references \url{https://influxdb.com/}
#' @section structure of configuration file:
#' A configuration file may contain several connection settings. Each setting 
#' has the following structure:\cr
#' ```
#' [group]
#' scheme=http
#' host=localhost  
#' port=8086
#' user=username
#' pass=password  
#' path=/
#' ```
influx_connection <-  function(scheme = c("http", "https"),
                               host = "localhost",
                               port = 8086,
                               user = "user",
                               pass = "pass",
                               path = "/",
                               group = NULL,
                               verbose = FALSE,
                               config_file = "~/.influxdb.cnf") {
  # if group name is given, get db credentials from config file
  if (!is.null(group)) {
    if (file.exists(config_file)) {
      # open file
      con <- file(config_file, open = "r")
      
      # read file
      lines <- readLines(con)
      
      # find line with groupname
      grp <- grep(paste0("[", group, "]"), x = lines, fixed = T)
      
      # catch the case if group name is not found
      if (identical(grp, integer(0)))
        stop("Group does not exist in config file.")
      
      # get db credentials
      scheme <- gsub("scheme=", "", grep("scheme=", lines[(grp + 1):(grp + 6)], fixed = T, value = T))
      host <- gsub("host=", "", grep("host=", lines[(grp + 1):(grp + 6)], fixed = T, value = T))
      port <- as.numeric(gsub("port=", "", grep("port=", lines[(grp + 1):(grp + 6)], fixed = T, value = T)))
      user <- gsub("user=", "", grep("user=", lines[(grp + 1):(grp + 6)], fixed = T, value = T))
      pass <- gsub("pass=", "", grep("pass=", lines[(grp + 1):(grp + 6)], fixed = T, value = T))
      path <- gsub("path=", "", grep("path=", lines[(grp + 1):(grp + 6)], fixed = T, value = T))
      
      # close file connection
      close(con)
      
    } else{
      stop("Config file does not exist.")
      
    }
    
  }
  
  # create list of server connection details
  influxdb_srv <-
    list(
      scheme = match.arg(scheme, c("http", "https")),
      host = host,
      port = port,
      user = user,
      pass = pass,
      path = path
    )
  
  # submit test ping
  response <- httr::GET(
    url = "",
    scheme = influxdb_srv$scheme,
    hostname = influxdb_srv$host,
    port = influxdb_srv$port,
    path = paste0(path, "ping"),
    httr::timeout(5)
  )
  
  # print url
  if (verbose)
    print(response$url)
  
  # Check for communication errors
  if (response$status_code != 204) {
    if (length(response$content) > 0)
      warning(rawToChar(response$content))
    stop("Influx connection failed with HTTP status code ",
         response$status_code)
  }
  
  # print server response
  message(httr::http_status(response)$message)
  invisible(influxdb_srv)
}

#' @title Ping an influxdb server
#' @description This function pings an influxdb server 
#' (e.g. for connection testing)
#' @inheritParams influx_query
#'
#' @return A tibble with server information.
#' @rdname influx_ping
#' @export
#' @seealso \code{\link[xts]{xts}}, \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
influx_ping <- function(con) {
  # submit ping
  response <- httr::GET(
    url = "",
    scheme = con$scheme,
    hostname = con$host,
    port = con$port,
    path = paste0(con$path, "ping")
  )
  
  res <- response$all_headers %>%
    purrr::flatten() %>%
    purrr::map_at("headers", tibble::as_tibble) %>%
    as.data.frame() %>%
    tibble::as_tibble()
  
  return(res)
  
}

#' @title send POST to an InfluxDB server
#' @description This function sends POST to an InfluxDB server. It is not 
#' exported and only used for some helper functions within this package.
#' @inheritParams influx_query
#' @return A tibble or NULL
#' @references \url{https://influxdb.com/}
#' @keywords internal
influx_post <- function(con,
                        query = "") {

  # create query based on function parameters
  q <- list(u = con$user, p = con$pass)
  
  # add query
  q <- c(q, q = query)
  
  # submit POST
  response <- tryCatch(
    httr::POST(
      url = "",
      scheme = con$scheme,
      hostname = con$host,
      port = con$port,
      path = paste0(con$path, "query"),
      query = q
    ),
    error = function(e) {
      print(e)
      return(NULL)
    }
  )
  
  # if curl fails return NULL
  if (is.null(response)) {
    return(NULL)
  }
  
  # Check for communication errors
  check_srv_comm(response)

  response <- rawToChar(response$content) %>%  # convert to chars
    purrr::map(response_to_list) %>%
    purrr::map_df(post_list_to_tibble)
  
  # if everything is OK, there won't be columns such as "error" or "messages"..
  if (any(c("error", "messages") %in% colnames(response))) {
    # return result tbl visible
    return(response)
  }
  
  return(NULL)
}


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
  response <- tryCatch(httr::GET(url = "",
                                 scheme = con$scheme,
                                 hostname = con$host,
                                 port = con$port,
                                 path = paste0(con$path, "query"),
                                 query = q),
                       error = function(e) {print(e); return(NULL)})
  
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

