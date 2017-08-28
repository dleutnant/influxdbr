#' @title Write an xts object to an InfluxDB server
#' @description This function writes an xts object to an InfluxDB server.
#' Columnnames of the xts object are used as InfluxDB's field keys,
#' xts's coredata represent field values. Attributes are preserved and written
#' as tag keys and values, respectively.
#' @inheritParams influx_query
#' @inheritParams influx_wirte
#' @param df The data.frame object to write to an InfluxDB server.
#' @param time_col 
#' @param tag_col
#' @param field_col
#' @return A list of server responses.
#' @rdname influx_write
#' @export
#' @seealso \code{\link[xts]{xts}}, \code{\link[influxdbr]{influx_connection}}
#' @references \url{https://docs.influxdata.com/influxdb/}
influx_write_df <- function(con,
                            db,
                            df,
                            time_col = "time", 
                            tag_cols = NULL,
                            field_cols = NULL,
                            measurement = NULL,
                            rp = NULL,
                            precision = c("s", "ns", "u", "ms", "m", "h"),
                            consistency = c(NULL, "one", "quroum", "all", "any"),
                            max_points = 5000,
                            use_integers = FALSE) { 
  

}


# method to check whether the provided data.frame can be converted to 
# InfluxDB specific line protocol
# function is not exported
#' @keywords internal
validate_df <- function(x, time_col = NULL, tag_col = NULL, field_col = NULL) {
  
  # if x has either "time" column or no other  then
  # time column is of type POSIXct
  # no NA or NULL in time
  
  # if has "tag" columns, then 
  # do cols exist?
  # tag columns are all string columns, i.e. must be convertable
  
  # if has "value" columns, i.e. cols exist

  
  
}

.df_to_influxdb_line_protocol <- function(x,
                                          measurement,
                                          precision = precision,
                                          use_integers = FALSE) {
  
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
  
  measurement <- "velocity"
  tag_cols <- paste("tag", c("one", "two"), sep = "_")
  field_cols <- paste("field", c("chr", "float", "bool"), sep = "_")
  time_col <- "time"
  
  # measurement
  measurement <- tibble::tibble(measurement = rep("asdf", nrow(df)))
  
  # tags
  if (!is.null(tag_cols)) {
    tags <- df %>%
      dplyr::select(!!tag_cols) %>%
      purrr::imap_dfr( ~ paste(.y, .x, sep = "=")) %>% 
      tidyr::unite(col = "tags", dplyr::everything(), sep = ",") %>% 
      dplyr::mutate(tags = paste(",", tags, sep = ""))
    
  } else {
    tags <- NULL
  }
  
  # values
  values <- df %>%
    dplyr::select(!!field_cols) %>% # TODO: if field_cols not given than use ALL
    purrr::imap_dfr( ~ paste(.y, .x, sep = "=")) %>% 
    tidyr::unite(col = "values", dplyr::everything(), sep = ",") %>% 
    dplyr::mutate(values = paste(" ", values, sep = ""))
  
  # time
  if (!is.null(time_col)) {
    time <- df %>% 
      dplyr::select(!!time_col) %>% 
      dplyr::rename(time = !!time_col) %>% 
      dplyr::mutate(time = format(as.numeric(time) * div,
                                  scientific = FALSE)) %>% 
      dplyr::mutate(time = paste(" ", time, sep = ""))
    
  } else {
    time <- NULL
  }

  dplyr::bind_cols(measurement,
                   tags,
                   values, 
                   time) %>% 
    tidyr::unite("line_protocol",
                 dplyr::everything(), 
                 sep = "")
  
}






