#' @title Write a data.frame object to an InfluxDB server
#' @description This function writes a data.frame object to an InfluxDB server.
#' Columns may contain times and both tag and field values. 
#' Columnnames of the data.frame object are used as InfluxDB's tag and field keys.
#' 
#' @inheritParams influx_query
#' @inheritParams influx_write
#' @param x The data.frame object to write to an InfluxDB server.
#' @param time_col A character scalar naming the time index column.
#' @param tag_cols A character vector naming tag columns.
#' @return A list of server responses.
#' @rdname influx_write_df
#' @export
#' @seealso \code{\link[xts]{xts}}, \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
influx_write_df <- function(con,
                            db,
                            measurement = NULL,
                            x,
                            time_col = NULL, 
                            tag_cols = NULL,
                            rp = NULL,
                            precision = c("s", "ns", "u", "ms", "m", "h"),
                            consistency = c(NULL, "one", "quroum", "all", "any"),
                            max_points = 5000,
                            use_integers = FALSE) { 

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
  
  # split xts object into a list of xts objects to reduce batch size
  list_of_df <- suppressWarnings(split(x,
                                       rep(1:ceiling((nrow(x) / max_points)),
                                       each = max_points))
                                 )
  
  # conversion to influxdb line protocol and submit post
  response <- list_of_df %>% 
    purrr::map( ~ .df_to_influxdb_line_protocol(df = ., 
                                                measurement = measurement,
                                                tag_cols = tag_cols,
                                                time_col = time_col,
                                                precision = precision,
                                                use_integers = use_integers) %>% 
                  httr::POST(body = ., 
                             url = "",
                             httr::timeout(60),
                             scheme = con$scheme,
                             hostname = con$host,
                             port = con$port,
                             path = paste0(con$path, "write"),
                             query = q) %>% 
                  .check_srv_comm(.)
                )

  invisible(response)

}

# method to convert a data.frame object to InfluxDB specific line protocol
# function is not exported
#' @keywords internal
.df_to_influxdb_line_protocol <- function(df,
                                          measurement,
                                          tag_cols = NULL,
                                          time_col = NULL,
                                          precision = precision,
                                          use_integers = FALSE) {
  
  # stop if data.frame provided contains NA's
  if (!all(!purrr::map_lgl(df, ~ any(is.na(.))))) {
    
    print(df %>% 
      purrr::map( ~ which(is.na(.), arr.ind = T)) %>% # transform to logical 
      purrr::keep( ~ length(.x) > 0)) # discard integer(0)

    stop("Handling of NA's in data.frames is currently not supported. 
         Rows containing NA's are given above!")  
    
  }
  
  # measurement
  tbl_measurement <- tibble::tibble(measurement = rep(measurement, 
                                                      times = nrow(df)))
  
  # TAG SET (are not necessary)
  if (!is.null(tag_cols)) {
    tbl_tags <- df %>%
      # select only tag column
      dplyr::select(dplyr::one_of(tag_cols)) %>%
      # handling of special character in column names
      dplyr::rename_all(dplyr::funs(gsub(pattern = "[ ]", replacement = "\\\\ ", .))) %>% 
      dplyr::rename_all(dplyr::funs(gsub(pattern = "[,]", replacement = "\\\\,", .))) %>% 
      dplyr::rename_all(dplyr::funs(gsub(pattern = "[=]", replacement = "\\\\=", .))) %>% 
      # create tag set
      purrr::imap_dfr( ~ paste(.y, .x, sep = "=")) %>% 
      tidyr::unite(col = "tags", dplyr::everything(), sep = ",") %>% 
      dplyr::mutate(tags = paste(",", tags, sep = ""))
    
  } else {
    tbl_tags <- NULL
  }
  
  # for dquotes in fields of type character
  options("useFancyQuotes" = FALSE)
  
  # FIELD SET
  tbl_values <- df %>%
    # use all columns as fields except for tags and time 
    dplyr::select(-dplyr::one_of(tag_cols, time_col, "time")) %>%
    # double quote character columns
    dplyr::mutate_if(., is.character, base::dQuote) %>% 
    # remove ws
    dplyr::mutate_if(., is.character, gsub, pattern = "^\\s+|\\s+$", replacement = "") %>% 
    # add i in case for integers
    `if`(use_integers, dplyr::mutate_if(., is.integer, paste, "i", sep=""), .) %>%
    # create field set
    purrr::imap_dfr( ~ paste(.y, .x, sep = "=")) %>% 
    tidyr::unite(col = "values", dplyr::everything(), sep = ",") %>% 
    # add leading ws
    dplyr::mutate(values = paste(" ", values, sep = ""))
  
  # TIME (is not necessary, server time is used if not given)
  if (!is.null(time_col)) {
    tbl_time <- df %>% 
      # select time column if provided 
      dplyr::select(dplyr::one_of(time_col)) %>% 
      # rename for easier coding
      dplyr::rename(time = !!time_col) %>% 
      dplyr::mutate(time = format(as.numeric(time) * .get_precision_divisor(precision),
                                  scientific = FALSE)) %>% 
      dplyr::mutate(time = paste(" ", time, sep = ""))
    
  } else {
    tbl_time <- NULL
  }

  # merge all columns back to one tibble and make one chr vector
  influxdb_line_protocol <- dplyr::bind_cols(tbl_measurement,
                                             tbl_tags,
                                             tbl_values, 
                                             tbl_time) %>% 
    tidyr::unite("line_protocol",
                 dplyr::everything(), 
                 sep = "") %>% 
    purrr::flatten_chr() %>% 
    paste(., collapse = "\n")
  
  # invisibly return converted line protocol
  invisible(influxdb_line_protocol)
  
}






