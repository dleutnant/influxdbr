#' @title Create an influxdb_connection object
#' @description Create an influxdb_connection object by specifying server
#' connection details. Credentials can also be saved and accessed through a config file.
#' @param scheme The scheme to use, either http or https. Defaults to http.
#' @param host Hostname of the InfluxDB server. Defaults to localhost
#' @param port numerical. Port number of the InfluxDB server. Defaults to 8086.
#' @param user username The username to use. Defaults to "user"
#' @param pass password The password to use. Defaults to "pass".
#' @param path The prefix path on which the InfluxDB is running. Can be useful in proxy situations.
#' @param group The group to use within the config file.
#' @param verbose logical. Provide additional details?
#' @param config_file The configuration file to be used if `group` is
#' specified.
#' @rdname influx_connection
#' @export
#' @references \url{https://influxdb.com/}
#' @section structure of configuration file:
#' A configuration file may contain several connection settings. Each setting has 
#' the following structure:\cr
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
#' @description This function pings an influxdb server (e.g. for connection testing)
#' @inheritParams influx_query
#'
#' @return A list of server information.
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
  
  
  return(response$all_headers)
  
}

#' @title send POST to an InfluxDB server
#' @description This sends POST to an InfluxDB server. It is not exported and only
#' used for some helper functions within this package.
#' @inheritParams influx_query
#' @param query The InfluxDB post to be sent.
#' @return A tibble or NULL
#' @references \url{https://influxdb.com/}
#' @keywords internal
influx_post <- function(con,
                        query = "") {
  if (is.null(con)) {
    warning("Connection object is NULL.")
    return(NULL)
  }
  
  # create query based on function parameters
  q <- list(u = con$user, p = con$pass)
  
  # add query
  q <- c(q, q = query)
  
  # submit POST
  response <- tryCatch(
    httr::POST(
      url = "",
      scheme = "http",
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
  if (!response$status_code %in% c(200, 204)) {
    response_data <- jsonlite::fromJSON(rawToChar(response$content))
    warning(paste("http:", response_data$error), call. = FALSE)
    return(NULL)
  }
  
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
#' @param simplifyList logical. If only one series is returned, the result can be
#' flatten to directly get either a tibble or an xts object (instead of a list)
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
  if (is.null(con)) {
    warning("Connection object is NULL.")
    return(NULL)
  }
  
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
  if (!response$status_code %in% c(200, 204)) {
    response_data <- jsonlite::fromJSON(rawToChar(response$content))
    warning(paste("http:", response_data$error), call. = FALSE)
    return(NULL)
  }

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

#' @title Write an xts object to an InfluxDB server
#' @description This function writes an xts object to an InfluxDB server.
#' Columnnames of the xts object are used as InfluxDB's field keys,
#' xts's coredata represent field values. Attributes are preserved and written
#' as tag keys and values, respectively.
#' @inheritParams influx_query
#' @param xts The xts object to write to an InfluxDB server.
#' @param measurement Sets the name of the measurement.
#' @param rp Sets the target retention policy for the write. If not present the
#' default retention policy is used.
#' @param precision Sets the precision of the supplied Unix time values
#' ("s", "ns", "u", "ms", "m", "h"). If not present timestamps are assumed to be
#' in seconds.
#' @param consistency Set the number of nodes that must confirm the write.
#' If the requirement is not met the return value will be partial write
#' if some points in the batch fail, or write failure if all points in the batch
#' fail.
#' @param max_points Defines the maximum points per batch (defaults to 5000).
#' @param use_integers Should integers (instead of doubles) be written if present?
#' @return A list of server responses.
#' @rdname influx_write
#' @export
#' @seealso \code{\link[xts]{xts}}, \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
influx_write <- function(con,
                         db,
                         xts,
                         measurement = NULL,
                         rp = NULL,
                         precision = c("s", "ns", "u", "ms", "m", "h"),
                         consistency = c(NULL, "one", "quroum", "all", "any"),
                         max_points = 5000,
                         use_integers = FALSE) {
  # development options
  performance <- FALSE
  
  # create query based on function parameters
  q <- list(db = db,
            u = con$user,
            p = con$pass)
  
  # add precision parameter
  precision <- match.arg(precision)
  q <- c(q, precision = precision)
  
  # add retention policy
  if (!is.null(rp))
    q <- c(q, rp = rp)
  
  # add consistency parameter
  if (!is.null(consistency)) {
    q <- c(q, consistency = match.arg(consistency))
  }
  
  # get no of points for performance analysis
  no_of_points <- nrow(xts)
  
  # split xts object into a list of xts objects to reduce batch size
  list_of_xts <- suppressWarnings(split(xts,
                                        rep(1:ceiling((nrow(xts) / max_points)
                                        ),
                                        each = max_points)))
  
  # reclass xts objects (became "zoo" in previous split command)
  list_of_xts <- lapply(list_of_xts, xts::as.xts)
  
  # reassign attributes to elements of list_of_xts
  # (got lost in previous split command)
  for (i in seq_len(length(list_of_xts))) {
    xts::xtsAttributes(list_of_xts[[i]]) <- xts::xtsAttributes(xts)
  }
  
  if (performance)
    start <- Sys.time()
  
  res <- lapply(list_of_xts, function(x) {
    # convert xts to line protocol
    influxdb_line_protocol <-
      .xts_to_influxdb_line_protocol(
        xts = x,
        use_integers = use_integers,
        measurement = measurement,
        precision = precision
      )
    
    # submit post
    response <- httr::POST(
      url = "",
      httr::timeout(60),
      scheme = con$scheme,
      hostname = con$host,
      port = con$port,
      path = paste0(con$path, "write"),
      query = q,
      body = influxdb_line_protocol
    )
    
    
    # Check for communication errors
    if (response$status_code < 200 || response$status_code >= 300) {
      if (length(response$content) > 0)
        stop(rawToChar(response$content), call. = FALSE)
    }
    
    # assign server response to list "res"
    rawToChar(response$content)
    
  })
  
  if (performance)
    message(paste(
      "Wrote",
      no_of_points,
      "points in",
      Sys.time() - start,
      "seconds."
    ))
  
  invisible(res)
  
}

# method to convert an xts object to InfluxDB specific line protocol
# function is not exported
#' @keywords internal
.xts_to_influxdb_line_protocol <- function(xts,
                                           measurement,
                                           precision = precision,
                                           use_integers = FALSE) {
  # development options
  performance <-  FALSE
  
  if (performance)
    start <- Sys.time()
  
  # catch error no XTS object
  if (!xts::is.xts(xts))
    stop("Object is not an xts-object.")
  # catch error NULL colnames
  if (any(is.null(colnames(xts))))
    stop("colnames(xts) is NULL.")
  # catch error nrow
  if (nrow(xts) == 0)
    stop("nrow(xts) is 0.")
  
  # remove rows with NA's only
  xts <- xts[rowSums(is.na(xts)) != ncol(xts),]
  
  # take only valid attributes
  valid_attr <- which(xts::xtsAttributes(xts) != "")
  
  # extract tag keys and tag values
  tag_keys <- names(xts::xtsAttributes(xts)[valid_attr])
  tag_values <- xts::xtsAttributes(xts)[valid_attr]
  
  # handle commas and spaces in values
  tag_values <- gsub(pattern = "[ ]", replacement = "\\\\ ", x = tag_values)
  tag_values <- gsub(pattern = "[,]", replacement = "\\\\,", x = tag_values)
  tag_values <- gsub(pattern = "[|]", replacement = "\\\\|", x = tag_values)
  
  # handle empty values in keys
  tag_values <- gsub(pattern = "numeric\\(0\\)|character\\(0\\)",
                     replacement = "NA",
                     x = tag_values)
  
  # merge tag keys and values
  tag_key_value <-
    paste(tag_keys, tag_values, sep = "=", collapse = ",")
  
  # create time vector
  div <- switch(
    precision,
    "ns" = 1e+9,
    "u" = 1e+6,
    "ms" = 1e+3,
    "s" = 1,
    "m" = 1 / 60,
    "h" = 1 / (60 * 60)
  )
  
  time <- format(as.integer(as.numeric(zoo::index(xts)) * div), 
                 scientific = FALSE)
  
  # make sure all integers end with "i", this also sets mode to "character"
  # s. https://github.com/influxdb/influxdb/issues/3519
  if ((use_integers == TRUE) & is.numeric(xts)) {
    if (all(xts == floor(xts))) {
      xts[, ] <- sapply(seq_len(ncol(xts)), function(x)
        paste(xts[, x],
              "i",
              sep = ""))
    }
    
  } else {
    if (!is.numeric(xts)) {
      # add quotes if matrix contains no numerics i.e. -> characters
      options("useFancyQuotes" = FALSE)
      xts[, ] <-
        sapply(seq_len(ncol(xts)), function(x)
          base::dQuote(xts[, x]))
      # trim leading and trailing whitespaces
      xts <- gsub("^\\s+|\\s+$", "", xts)
    }
    
  }
  
  # assign columnname to each element
  values <- sapply(seq_len(ncol(xts)),
                   function(x)
                     paste(colnames(xts)[x],
                           zoo::coredata(xts)[, x],
                           sep = "="))
  
  # set R's NA values to a dummy string which can be removed easily
  # -> influxdb doesn't handle NA values
  # TODO: What if columnname contains "NA" ?
  values[grepl("NA", values)] <- "NA_to_remove"
  
  # If values have only one row, 'apply' will result in a dim error.
  # This occurs if the previous 'sapply' result a character vector.
  # Thus, check if a conversion is required:
  if (is.null(dim(values))) {
    dim(values) <- length(values)
  }
  
  # paste and collapse rows
  values <- apply(values, 1, paste, collapse = ",")
  
  # remove dummy strings
  values <- gsub(",NA_to_remove|NA_to_remove,", "", values)
  
  # no tags assigned
  if (is.null(tag_values) | identical(character(0), tag_values)) {
    influxdb_line_protocol <- paste(measurement,
                                    values,
                                    time,
                                    collapse = "\n")
  } else {
    influxdb_line_protocol <- paste(
      measurement,
      paste(",", tag_key_value, sep = ""),
      " ",
      values,
      " ",
      time,
      sep = "",
      collapse = "\n"
    )
  }
  
  if (performance)
    message(paste(
      "Converted",
      length(xts),
      "points in",
      Sys.time() - start,
      "seconds."
    ))
  
  # invisibly return InfluxDB line protocol string
  invisible(influxdb_line_protocol)
}


# method to convert the line protocol to a data.frame
# function is not exported
#' @keywords internal
.influxdb_line_protocol_to_array <- function(x) {
  # split by ","
  splitted_string <- unlist(strsplit(x, split = ","))
  
  # extract measurement name
  measurement_df <- data.frame(measurement = splitted_string[1])
  
  # extract tags and tag values
  if (identical(splitted_string[-1], character(0))) {
    warning(paste("measurement does not have any attributes:", x))
    return(NULL)
  }
  
  df <- strsplit(x = splitted_string[-1], split = "=")
  df <- do.call(cbind, df)
  
  # create result df with tag names as colnames
  result <- data.frame(t(df[2, ]), stringsAsFactors = FALSE)
  colnames(result) <- df[1, ]
  
  # combine measurement name and tagkeys and tagvalues
  result <- cbind(measurement_df, result)
  
  return(result)
  
}
