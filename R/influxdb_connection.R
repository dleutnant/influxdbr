#' @title Create an influxdb_connection object
#' @description Create an influxdb_connection object by specifying server
#' connection details. Credentials can also be saved and accessed through a 
#' config file.
#' @param scheme The scheme to use, either http or https. Defaults to http.
#' @param host Hostname of the InfluxDB server. Defaults to localhost.
#' @param port numerical. Port number of the InfluxDB server. Defaults to 8086.
#' @param user username The username to use. Defaults to "user".
#' @param pass password The password to use. Defaults to "pass".
#' @param path The prefix path on which the InfluxDB is running. Can be useful 
#' in proxy situations.
#' @param group The group to use within the config file.
#' @param verbose logical. Provide additional details?
#' @param config_file The configuration file to be used if `group` is
#' specified.
#' @param curl_options Additional curl arguments created with \code{\link[httr]{config}} 
#' (e.g. httr::config(verbose = TRUE, timeout = 5, ssl_verifypeer = FALSE)). 
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
                               config_file = "~/.influxdb.cnf",
                               curl_options = NULL) {
  # if group name is given, get db credentials from config file
  if (!is.null(group)) {
    if (file.exists(config_file)) {
      # open file
      con <- file(config_file, open = "r")
      
      # read file
      lines <- readLines(con)
      
      # find line with groupname
      grp <- grep(sprintf("^ *\\[%s\\]", group), x = lines)
      
      # catch the case if group name is not found
      if (identical(grp, integer(0)))
        stop("Group does not exist in config file.")

      if (length(grp) > 1)
        stop(sprintf("Multiple groups with the same name (%s) in config file.", group))

      the_lines <- lines[(grp + 1):min(grp + 6, length(lines))]
      
      # get db credentials
      scheme <- gsub("scheme=", "", grep("scheme=", the_lines, fixed = T, value = T))
      host <- gsub("host=", "", grep("host=", the_lines, fixed = T, value = T))
      port <- as.numeric(gsub("port=", "", grep("port=", the_lines, fixed = T, value = T)))
      user <- gsub("user=", "", grep("user=", the_lines, fixed = T, value = T))
      pass <- gsub("pass=", "", grep("pass=", the_lines, fixed = T, value = T))
      path <- gsub("path=", "", grep("path=", the_lines, fixed = T, value = T))
      
      # close file connection
      close(con)
      
    } else{
      stop("Config file does not exist.")
      
    }
    
  }
  
  # check if curl_options are of class "request"
  if (!is.null(curl_options) && !inherits(curl_options, "request")) {
    stop(paste("Please provide curl options with the {httr} package,", 
               "e.g. curl_options = httr::config(verbose = TRUE)"),
         call. = FALSE)
  }
  
  # create list of server connection details
  influxdb_srv <-
    list(
      scheme = match.arg(scheme, c("http", "https")),
      host = host,
      port = port,
      user = user,
      pass = pass,
      path = path,
      config = curl_options
    )
  
  # submit test ping
  response <- httr::GET(
    url = "",
    scheme = influxdb_srv$scheme,
    hostname = influxdb_srv$host,
    port = influxdb_srv$port,
    path = paste0(influxdb_srv$path, "ping"),
    config = influxdb_srv$config
  )
  
  # print url
  if (verbose) print(response$url)
  
  # Check for communication errors
  check_srv_comm(response)
  
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
#' @seealso \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
influx_ping <- function(con) {
  
  # submit ping
  response <- httr_GET(con = con, endpoint = "ping")
  
  res <- response$all_headers %>%
    purrr::flatten() %>%
    purrr::map_at("headers", tibble::as_tibble) %>%
    as.data.frame() %>%
    tibble::as_tibble()
  
  return(res)
  
}





