#' Create an influxdb_connection object
#'
#' @title influx_connection
#' @param host hostname
#' @param port numerical. port number
#' @param user username
#' @param pass password
#' @param group groupname
#' @param verbose logical. Provide additional details?
#' @param config_file The configuration file to be used if \code{groupname} is specified.
#' @rdname influx_connection
#' @export
#' @author Dominik Leutnant (\email{leutnant@@fh-muenster.de})
#' @references \url{https://influxdb.com/}
influx_connection <-  function(host=NULL,
                               port=NULL,
                               user="user",
                               pass="pass",
                               group=NULL,
                               verbose=FALSE, config_file="~/.influxdb.cnf") {

  #' group file looks like:
  #' [groupname]
  #' host=localhost
  #' port=8086
  #' user=username
  #' pass=password

  # if group name is given, get db credentials from config file
  if (!is.null(group)) {

    if (file.exists(config_file)) {

      # open file
      con <- file(config_file, open="r")

      # read file
      lines <- readLines(con)

      # find line with groupname
      grp <- grep(paste0("[",group,"]"), x = lines, fixed = T)

      # catch the case if group name is not found
      if (identical(grp, integer(0))) stop ("Group does not exist in config file.")

      # get db credentials
      host <- gsub("host=", "", grep("host=", lines[(grp+1):(grp+4)], fixed=T, value = T))
      port <- as.numeric(gsub("port=", "", grep("port=", lines[(grp+1):(grp+4)], fixed=T, value = T)))
      user <- gsub("user=", "", grep("user=", lines[(grp+1):(grp+4)], fixed=T, value = T))
      pass <- gsub("pass=", "", grep("pass=", lines[(grp+1):(grp+4)], fixed=T, value = T))

      # close file connection
      close(con)

    } else{

      stop ("Config file does not exist.")

    }

  }

  # create list of server connection details
  influxdb_srv <- list(host=host, port=port, user=user, pass=pass)

  # submit test ping
  response <- httr::GET(url="", scheme="http", hostname=influxdb_srv$host, port=influxdb_srv$port, path="ping", httr::timeout(5))

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

#' Write an xts object to an influxdb server
#'
#'
#' @title influx_write
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param db The name of the database.
#' @param xts The xts object to write to an influxdb server.
#' @param measurement The name of the measurement.
#' @param precision Specifies the timestamp format ("default", "n", "u", "ms", "s", "m", "h").
#' @param max_points Defines the maximum points per batch.
#' @param digits How many digits are to be used.
#' @param performance logical. Print performance measurements.
#' @return A list of server responses.
#' @rdname influx_write
#' @export
#' @author Dominik Leutnant (\email{leutnant@@fh-muenster.de})
#' @seealso \code{\link[xts]{xts}}, \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://influxdb.com/docs/v0.9/guides/writing_data.html}
influx_write <- function(con,
                         db,
                         xts,
                         measurement=NULL,
                         precision="s",
                         max_points=5000,
                         digits=5,
                         performance=F) {

  #curl -i -XPOST 'http://localhost:8086/write?db=mydb'
  #-d 'cpu_load_short,host=server01,region=us-west value=0.64 cpu_load_short,
  #host=server02,region=us-west value=0.55 1422568543702900257 cpu_load_short,
  #direction=in,host=server01,region=us-west value=23422.0 1422568543702900257'

  # create query based on function parameters
  ## TODO: handle different precision
  q <- list(db = db, u = con$user, p = con$pass, precision = "s")

  # split xts object into a list of xts objects to reduce batch size
  list_of_xts <- suppressWarnings( split( xts,
                                          rep(1:ceiling((nrow(xts)/max_points)),
                                              each=max_points)))

  # reclass xts objects (became "zoo" in previous split command)
  list_of_xts <- lapply(list_of_xts, xts::as.xts)

  # reassign attributes to elements of list_of_xts (got lost in previous split command)
  for(i in seq_len(length(list_of_xts))) {
    xts::xtsAttributes(list_of_xts[[i]]) <- xts::xtsAttributes(xts)
  }

  res <- lapply(list_of_xts, function(x) {

    # convert xts to line protocol
    influxdb_line_protocol <- .xts_to_influxdb_line_protocol(xts = x,
                                                             digits = digits,
                                                             measurement = measurement,
                                                             performance = performance)


    if (performance) start <- Sys.time()

    # submit post
    response <- httr::POST(url="", httr::timeout(60),
                           scheme="http",
                           hostname=con$host,
                           port=con$port,
                           path="write",
                           query=q, body = influxdb_line_protocol)


    # Check for communication errors
    if (response$status_code < 200 || response$status_code >= 300) {
      if (length(response$content) > 0)
        warning(rawToChar(response$content))
      stop("Influx write failed with HTTP status code ", response$status_code)
    }

    if (performance) message( paste("Wrote",
                                    length(x), "points in", Sys.time()-start,
                                    "seconds."))

    # assign server response to list "res"
    rawToChar(response$content)

  })

  invisible(res)

}

#' Query an influxdb server
#'
#'
#' @title influx_query
#' @param con An influx_connection object (s. \code{influx_connection}).
#' @param db The name of the database.
#' @param query The influxdb query to be sent.
#' @param timestamp_format Specifies the timestamp format
#' ("default" (=UTC), "n", "u", "ms", "s", "m", "h")
#' @param verbose logical. Provide additional details?
#' @param debug logical. For debugging purposes only.
#' @return A list of xts objects
#' @rdname influx_query
#' @export
#' @author Dominik Leutnant (\email{leutnant@@fh-muenster.de})
#' @seealso \code{\link[xts]{xts}}, \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://influxdb.com/docs/v0.9/guides/querying_data.html}
influx_query <- function(con,
                         db=NULL,
                         query="SELECT * FROM measurement",
                         timestamp_format = "default",
                         verbose=FALSE,
                         debug=FALSE) {

  #'http://localhost:8086/query' --data-urlencode "db=mydb"
  #'--data-urlencode "q=SELECT value FROM cpu_load_short WHERE region='us-west'"
  #'

  # handle different timestamp formats
  if (timestamp_format!="default") {
    if (timestamp_format %in% c("n", "u", "ms", "s", "m", "h")) {
      q <- c(q, epoch = timestamp_format)
    } else {
      stop("Unknown timestamp format.")
    }
  }

  # submit query
  response <- httr::GET(url="", scheme="http", hostname=con$host,
                        port=con$port, path="query", query = list(db = db,
                                                                  u = con$user,
                                                                  p = con$pass,
                                                                  q = query))

  # print url
  if (verbose) print(response$url)

  # Check for communication errors
  if (response$status_code < 200 || response$status_code >= 300) {
    if (length(response$content) > 0)
      warning(rawToChar(response$content))
    stop("Influx query failed with HTTP status code ", response$status_code)
  }

  # parse response from json
  response_data <- jsonlite::fromJSON(rawToChar(response$content),
                                      simplifyVector = TRUE,
                                      simplifyDataFrame = FALSE,
                                      simplifyMatrix = FALSE)

  # DEBUG OUTPUT
  if (debug) debug_influx_query_response_data <<- response_data

  # Check for database or time series error
  if (exists(x = "error", where = response_data)) {
    warning("Influx query returned error: ", response_data$error)
  }

  # Check if query returned results
  if (exists(x = "results", where = response_data)) {

    # create list of results
    list_of_results <- lapply(response_data$results, function(resultsObj) {

      # check if query returned series object(s)
      if (exists(x = "series", where = resultsObj)) {

        # create list of xts objects
        list_of_xts <- lapply(resultsObj$series, function(seriesObj) {

          # check if response contains values
          if (exists(x = "values", where = seriesObj)) {

            # extract values and columnnames
            values <- as.data.frame(unname(t(as.data.frame(seriesObj$values))),
                                    stringsAsFactors=FALSE)

          } else {

            stop("Influx query returned series with no values.")

          }

          # check if response contains columns
          if (exists(x = "columns", where = seriesObj)) {

            colnames(values) <- seriesObj$columns

          } else {

            stop("Influx query returned series with no columns.")

          }

          # check if response contains times
          if ("time" %in% colnames(values)) {

            # extract time vector to feed xts object
            if (timestamp_format!="default") {

              # to do: dealing with "millisecs" and "nanosecs"
              time <- as.POSIXct(values[,'time'], origin="1970-1-1")

            } else {

              time <- as.POSIXct(strptime(values[,'time'],
                                          format = "%Y-%m-%dT%H:%M:%SZ"))

            }

            # select all but time vector to feed xts object
            values <- values[, colnames(values) != 'time', drop=FALSE]

            # try to convert strings (type.convert needs characters!)
            values[] <- lapply(values, as.character)
            values[] <- lapply(values, type.convert, as.is=TRUE)

            # create xts object for each returned column
            res <- lapply(values, function(x) xts::xts(x = x, order.by = time))

            # assign colname and xtsAttributes
            for(i in seq_len(length(res))) {
              colnames(res[[i]]) <- colnames(values)[i]
              xts::xtsAttributes(res[[i]]) <- seriesObj$tags
            }

            # assign measurement name
            names(res) <- seriesObj$name

          } else {

            res <- values

          }

          return(res)

        })

        return(list_of_xts)

      } else {

        message("Influx query returned no series.")

        return(response_data)

      }

    })


  } else {

    warning("Influx query returned no results.")

  }

}

# method to convert an xts-object to influxdb specific line protocol
.xts_to_influxdb_line_protocol <- function(xts,
                                           measurement,
                                           digits=5,
                                           performance=FALSE){

  if (performance) start <- Sys.time()

  # catch error no XTS object
  if (!xts::is.xts(xts)) stop ("Object is not an xts-object.")
  # catch error NULL colnames
  if (any(is.null(colnames(xts)))) stop ("colnames(xts) is NULL.")
  # catch error nrow
  if (nrow(xts)==0) stop ("nrow(xts) is 0.")

  # remove rows with NA's only
  xts <- xts[rowSums(is.na(xts))!=ncol(xts), ]

  # extract tag keys and tag values
  tag_keys <- names(xts::xtsAttributes(xts))
  tag_values <- xts::xtsAttributes(xts)

  # handle commas and spaces in values
  tag_values <- gsub(pattern = ",| ", replacement = "_", x = tag_values)

  # handle empty values in keys
  tag_values <- gsub(pattern = "numeric\\(0\\)|character\\(0\\)",
                   replacement = "NA", x = tag_values)

  # merge tag keys and values
  tag_key_value <- paste(tag_keys, tag_values, sep="=", collapse = ",")

  # create time vector
  time <- format(as.numeric(zoo::index(xts)), scientific = FALSE)

  # make sure all numerics contains "n" digits to ensure float64 type,
  # this also sets mode to "character",
  if (is.numeric(xts)) {
    xts[,] <-  format(round(xts, digits), nsmall=digits)
  } else {
    # add quotes if matrix contains strings
    options("useFancyQuotes" = FALSE)
    xts[,] <- sapply(seq_len(ncol(xts)), function(x) base::dQuote(xts[,x]))
  }

  # trim leading and trailing whitespaces
  xts <- gsub("^\\s+|\\s+$", "", xts)

  # assign columnname to each element
  values <- sapply(seq_len(ncol(xts)),
                   function(x) paste(colnames(xts)[x],
                                     zoo::coredata(xts)[,x],
                                     sep="="))

  # set R's NA values to a dummy string which can be removed easily
  # -> influxdb doesn't handle NA values
  # TODO: What if columnname contains "NA" ?
  values[grepl("NA", values)] <- "NA_to_remove"

  # paste and collapse rows
  values <- apply(values, 1, paste, collapse=",")

  # remove dummy strings
  values <- gsub(",NA_to_remove|NA_to_remove,", "", values)

  # no tags assigned
  if (is.null(tag_values) | identical(character(0), tag_values)){
    influxdb_line_protocol <- paste(measurement,
                                    values,
                                    time,
                                    collapse = "\n")
  } else {
    influxdb_line_protocol <- paste(measurement,
                                    paste(",", tag_key_value, sep=""), " ",
                                    values, " ", time, sep="",collapse = "\n")
  }

  if (performance) message( paste("Converted",
                                  length(xts), "points in", Sys.time()-start,
                                  "seconds."))



  # invisibly return influxdb line protocol string
  invisible(influxdb_line_protocol)
}
