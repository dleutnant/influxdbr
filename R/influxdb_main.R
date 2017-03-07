#' Create an influxdb_connection object
#'
#' @title influx_connection
#' @param scheme The scheme to use, either http or https. Defaults to http.
#' @param host Hostname of the InfluxDB server. Defaults to localhost
#' @param port numerical. Port number of the InfluxDB server. Defaults to 8086.
#' @param user username The username to use. Defaults to "user"
#' @param pass password The password to use. Defaults to "pass".
#' @param path The prefix path on which the InfluxDB is running. Can be useful in proxy situations.
#' @param group The group to use within the config file.
#' @param verbose logical. Provide additional details?
#' @param config_file The configuration file to be used if \code{groupname} is
#' specified.
#' @rdname influx_connection
#' @export
#' @references \url{https://influxdb.com/}
influx_connection <-  function(scheme = "http",
                               host = "localhost",
                               port = 8086,
                               user = "user",
                               pass = "pass",
                               path = "/",
                               group = NULL,
                               verbose = FALSE,
                               config_file = "~/.influxdb.cnf") {

  #' A group file looks like:
  #' [groupname]
  #' scheme=http
  #' host=localhost
  #' port=8086
  #' user=username
  #' pass=password
  #' path=/

  # if group name is given, get db credentials from config file
  if (!is.null(group)) {

    if (file.exists(config_file)) {

      # open file
      con <- file(config_file, open = "r")

      # read file
      lines <- readLines(con)

      # find line with groupname
      grp <- grep(paste0("[",group,"]"), x = lines, fixed = T)

      # catch the case if group name is not found
      if (identical(grp, integer(0))) stop("Group does not exist in config file.")

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
  influxdb_srv <- list(scheme = match.arg(scheme, c("http", "https")),
                       host = host,
                       port = port,
                       user = user,
                       pass = pass,
                       path = path)

  # submit test ping
  response <- httr::GET(url = "",
                        scheme = influxdb_srv$scheme,
                        hostname = influxdb_srv$host,
                        port = influxdb_srv$port,
                        path = paste0(path, "ping"),
                        httr::timeout(5))

  # print url
  if (verbose) print(response$url)

  # Check for communication errors
  if (response$status_code != 204) {
    if (length(response$content) > 0) warning(rawToChar(response$content))
    stop("Influx connection failed with HTTP status code ", response$status_code)
  }

  # print server response
  message(httr::http_status(response)$message)
  invisible(influxdb_srv)
}

#' Ping an influxdb server
#'
#' @title influx_ping
#' @param con An influx_connection object (s. \code{influx_connection}).
#'
#' @return A list of server information.
#' @rdname influx_ping
#' @export
#' @seealso \code{\link[xts]{xts}}, \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
influx_ping <- function(con) {

  # submit ping
  response <- httr::GET(url = "",
                        scheme = con$scheme,
                        hostname = con$host,
                        port = con$port,
                        path = paste0(con$path, "ping"))


  return(response$all_headers)

}

#' Query an influxdb server
#'
#' @title influx_query
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param db Sets the name of the database.
#' @param query The influxdb query to be sent.
#' @param timestamp_format Sets the timestamp format
#' ("n", "u", "ms", "s", "m", "h").
#' @param return_xts logical. Sets the return type. If set to TRUE, xts objects
#' are returned, FALSE gives data.frames.
#' @param verbose logical. Provide additional details?
#'
#' @return A list of xts or data.frame objects.
#' @rdname influx_query
#' @export
#' @author Dominik Leutnant (\email{leutnant@@fh-muenster.de})
#' @seealso \code{\link[xts]{xts}}, \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
influx_query <- function(con,
                         db = NULL,
                         query = "SELECT * FROM measurement",
                         timestamp_format = c("n", "u", "ms", "s", "m", "h"),
                         return_xts = TRUE,
                         verbose = FALSE) {

  if (is.null(con)) {
    warning("Connection object is NULL.")
    return(NULL)
  }

  # development options
  debug <-  FALSE

  # create query based on function parameters
  q <- list(db = db, u = con$user, p = con$pass)

  # handle different timestamp formats
  timestamp_format <- match.arg(timestamp_format)
  q <- c(q, epoch = timestamp_format)

  # add query
  q <- c(q, q = query)

  if (verbose) print(query)

  # submit query
  response <- tryCatch(httr::GET(url = "",
                                 scheme = "http",
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

  # print url
  if (verbose) print(response$url)

  # DEBUG OUTPUT
  if (debug) debug_influx_query_response_data <<- response

  # Check for communication errors
  if (!response$status_code %in% c(200,204)) {
    response_data <- jsonlite::fromJSON(rawToChar(response$content))
    warning(paste("http:", response_data$error), call. = FALSE)
    return(NULL)
  }

  # parse response from json
  response_data <- jsonlite::fromJSON(rawToChar(response$content),
                                      simplifyVector = FALSE,
                                      simplifyDataFrame = FALSE,
                                      simplifyMatrix = FALSE)

  # Check for database or time series error
  if (exists(x = "error", where = response_data)) {
    warning(response_data$error, call. = FALSE)
    return(NULL)
  }

  # Check if query returned results
  if (exists(x = "results", where = response_data)) {

    # create list of results
    list_of_results <- lapply(response_data$results, function(resultsObj) {

      # check if query returned series object(s) with errors
      if (exists(x = "error", where = resultsObj)) {

        warning(resultsObj$error, call. = FALSE)
        return(NULL)

      } else {

        # check if query returned series object(s)
        if (exists(x = "series", where = resultsObj)) {

          # create list of series
          list_of_series <- lapply(resultsObj$series, function(seriesObj) {

            # check if response contains values
            if (exists(x = "values", where = seriesObj)) {

              ### TODO: What if response contains chunked time series?

              # extract values and columnnames
              values <- as.data.frame(t(matrix(unlist(seriesObj$values),
                                               nrow = length(unlist(seriesObj$values[1])))),
                                     stringsAsFactors = FALSE,
                                     row.names = NULL)

              # convert columns to most appropriate type
              # type.convert needs characters!
              # TODO: CONSUMES A LOT OF TIME OF FUN CALL!!! ALTERNATIVES ?
              values[] <- lapply(values, as.character)
              values[] <- lapply(values, utils::type.convert, as.is = TRUE)

              # check if response contains columns
              if (exists(x = "columns", where = seriesObj)) {

                colnames(values) <- seriesObj$columns

                # check if response contains times
                # "Show Measurements", "Show Series", "Show Tag Keys"
                # and "Show Tag Values" queries do not provide "time"
                if ("time" %in% colnames(values)) {

                  # extract time vector to feed xts object...

                  # when dealing with "millisecs", "nanosecs" or ... we need
                  # a divisor:
                  div <- switch(timestamp_format,
                                  "n" = 1e+9,
                                  "u" = 1e+6,
                                  "ms" = 1e+3,
                                  "s" = 1,
                                  "m" = 1/60,
                                  "h" = 1/(60*60))

                  time <- as.POSIXct(values[,'time']/div, origin = "1970-1-1")

                  # select all but time vector to feed xts object
                  values <- values[, colnames(values) != 'time', drop = FALSE]

                  # should xts objects or data.frames be returned?
                  if (return_xts == TRUE) {

                    # create xts object for each returned column
                    # xts objects are based on matrix --> always one type only!
                    values <- lapply(values, function(x) xts::xts(x = x,
                                                                  order.by = time))

                    # assign colname and xtsAttributes
                    for (i in seq_len(length(values))) {
                      colnames(values[[i]]) <- seriesObj$columns[i + 1] # +1 to skip "time"
                      xts::xtsAttributes(values[[i]]) <- c(influx_query = query,
                                                           seriesObj$tags)
                    }

                    # assign measurement name
                    names(values) <- rep(seriesObj$name, length(values))

                    # return values as list of xts object(s)
                    return(values)

                  } else {

                    # return values as structure: data.frame with attributes
                    # with converted cols
                    values <- list( structure( cbind(time, values),
                                               influx_query = query,
                                               influx_tags = seriesObj$tags))

                    # assign measurement name
                    names(values) <- seriesObj$name

                    return(values)

                  }

                } else {

                  # return values as structure: data.frame with attributes
                  values <- list( structure( values,
                                             influx_query = query,
                                             influx_tags = seriesObj$tags))

                  # assign measurement name
                  names(values) <- seriesObj$name

                  return(values)

                }

              } else {

                warning("Influx query returned series with no columns.")

              }

            } else {

              warning("Influx query returned series with no values.")

            }

          })

          return(list_of_series)

        } else {

          #warning("Influx query returned no series.")

          return(NULL)

        }

      }

    })

    return(list_of_results)

  } else {

    warning("Influx query returned no results.")

    return(NULL)

  }

}

#' Write an xts object to an influxdb server
#'
#'
#' @title influx_write
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param db Sets the target database for the write.
#' @param xts The xts object to write to an influxdb server.
#' @param measurement Sets the name of the measurement.
#' @param rp Sets the target retention policy for the write. If not present the
#' default retention policy is used.
#' @param precision Sets the precision of the supplied Unix time values
#' ("n", "u", "ms", "s", "m", "h"). If not present timestamps are assumed to be
#' in nanoseconds. Currently only "s" is supported.
#' @param consistency Set the number of nodes that must confirm the write.
#' If the requirement is not met the return value will be partial write
#' if some points in the batch fail, or write failure if all points in the batch
#' fail.
#' @param max_points Defines the maximum points per batch.
#' @param use_integers Should integers (instead of doubles) be written if present?
#' @return A list of server responses.
#' @rdname influx_write
#' @export
#' @author Dominik Leutnant (\email{leutnant@@fh-muenster.de})
#' @seealso \code{\link[xts]{xts}}, \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
influx_write <- function(con,
                         db,
                         xts,
                         measurement = NULL,
                         rp = NULL,
                         precision = c("s", "n", "u", "ms", "m", "h"),
                         consistency = c(NULL, "one", "quroum", "all", "any"),
                         max_points = 5000,
                         use_integers = FALSE) {

  # development options
  performance <- FALSE

  # create query based on function parameters
  q <- list(db = db, u = con$user, p = con$pass)

  # add precision parameter
  q <- c(q, precision = match.arg(precision))

  # add retention policy
  if (!is.null(rp)) q <- c(q, rp = rp)

  # add consistency parameter
  if (!is.null(consistency)) {
    q <- c(q, consistency = match.arg(consistency))
  }

  # get no of points for performance analysis
  no_of_points <- nrow(xts)

  # split xts object into a list of xts objects to reduce batch size
  list_of_xts <- suppressWarnings( split( xts,
                                          rep(1:ceiling((nrow(xts)/max_points)),
                                              each = max_points)))

  # reclass xts objects (became "zoo" in previous split command)
  list_of_xts <- lapply(list_of_xts, xts::as.xts)

  # reassign attributes to elements of list_of_xts
  # (got lost in previous split command)
  for (i in seq_len(length(list_of_xts))) {
    xts::xtsAttributes(list_of_xts[[i]]) <- xts::xtsAttributes(xts)
  }

  if (performance) start <- Sys.time()

  res <- lapply(list_of_xts, function(x) {

    # convert xts to line protocol
    influxdb_line_protocol <- .xts_to_influxdb_line_protocol(xts = x,
                                                             use_integers = use_integers,
                                                             measurement = measurement)

    # submit post
    response <- httr::POST(url = "", httr::timeout(60),
                           scheme = con$scheme,
                           hostname = con$host,
                           port = con$port,
                           path = paste0(con$path, "write"),
                           query = q,
                           body = influxdb_line_protocol)


    # Check for communication errors
    if (response$status_code < 200 || response$status_code >= 300) {
      if (length(response$content) > 0)
        stop(rawToChar(response$content), call. = FALSE)
    }

    # assign server response to list "res"
    rawToChar(response$content)

  })

  if (performance) message(paste("Wrote", no_of_points, "points in",
                                 Sys.time() - start, "seconds."))

  invisible(res)

}

# method to convert an xts-object to influxdb specific line protocol
.xts_to_influxdb_line_protocol <- function(xts,
                                           measurement,
                                           use_integers = FALSE){

  # development options
  performance <-  FALSE

  if (performance) start <- Sys.time()

  # catch error no XTS object
  if (!xts::is.xts(xts)) stop("Object is not an xts-object.")
  # catch error NULL colnames
  if (any(is.null(colnames(xts)))) stop("colnames(xts) is NULL.")
  # catch error nrow
  if (nrow(xts) == 0) stop("nrow(xts) is 0.")

  # remove rows with NA's only
  xts <- xts[rowSums(is.na(xts)) != ncol(xts), ]

  # take only valid attributes
  valid_attr <- which(xts::xtsAttributes(xts) != "")

  # extract tag keys and tag values
  tag_keys <- names(xts::xtsAttributes(xts)[valid_attr])
  tag_values <- xts::xtsAttributes(xts)[valid_attr]

  # handle commas and spaces in values
  tag_values <- gsub(pattern = ",| ", replacement = "_", x = tag_values)

  # handle empty values in keys
  tag_values <- gsub(pattern = "numeric\\(0\\)|character\\(0\\)",
                     replacement = "NA", x = tag_values)

  # merge tag keys and values
  tag_key_value <- paste(tag_keys, tag_values, sep = "=", collapse = ",")

  # create time vector
  time <- format(as.numeric(zoo::index(xts)), scientific = FALSE)

  # make sure all integers end with "i", this also sets mode to "character"
  # s. https://github.com/influxdb/influxdb/issues/3519
  if ((use_integers == TRUE) & (all(xts == floor(xts)))) {

    xts[,] <- sapply(seq_len(ncol(xts)), function(x) paste(xts[,x],
                                                           "i",
                                                           sep = ""))
  } else {
    if (!is.numeric(xts)) {
      # add quotes if matrix contains no numerics i.e. -> characters
      options("useFancyQuotes" = FALSE)
      xts[,] <- sapply(seq_len(ncol(xts)), function(x) base::dQuote(xts[,x]))
      # trim leading and trailing whitespaces
      xts <- gsub("^\\s+|\\s+$", "", xts)
    }
  }

  # assign columnname to each element
  values <- sapply(seq_len(ncol(xts)),
                   function(x) paste(colnames(xts)[x],
                                     zoo::coredata(xts)[,x],
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
    influxdb_line_protocol <- paste(measurement,
                                    paste(",", tag_key_value, sep = ""), " ",
                                    values, " ", time, sep = "",collapse = "\n")
  }

  if (performance) message( paste("Converted",
                                  length(xts), "points in", Sys.time() - start,
                                  "seconds."))

  # invisibly return influxdb line protocol string
  invisible(influxdb_line_protocol)
}


.influxdb_line_protocol_to_array <- function(x) {

  # split by ","
  splitted_string <- unlist(strsplit(x, split = ","))

  # extract measurement name
  #measurement_name <- splitted_string[1]

  # extract tags and tag values
  df <- strsplit(x = splitted_string[-1], split = "=")
  df <- do.call(cbind, df)

  # create result df with tag names as colnames
  result <- data.frame(t(df[2,]), stringsAsFactors = FALSE)
  colnames(result) <- df[1,]

  return(result)

}
