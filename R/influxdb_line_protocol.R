# method to convert xts and data.frame objects to InfluxDB specific line protocol
# function is not exported
#' @keywords internal
convert_to_line_protocol <- function(x,
                                     measurement,
                                     precision,
                                     use_integers = FALSE,
                                     ...) {
  
  UseMethod("convert_to_line_protocol", x)
  
}

#' @keywords internal
convert_to_line_protocol.xts <- function(x,
                                         measurement,
                                         precision,
                                         use_integers = FALSE, 
                                         ...) {
  
  # catch error no XTS object
  if (!xts::is.xts(x))
    stop("Object is not an xts-object.")
  # catch error NULL colnames
  if (any(is.null(colnames(x))))
    stop("colnames(xts) is NULL.")
  # catch error nrow
  if (nrow(x) == 0)
    stop("nrow(xts) is 0.")
  
  # remove rows with NA's only
  x <- x[rowSums(is.na(x)) != ncol(x),]
  
  # take only valid attributes
  valid_attr <- which(xts::xtsAttributes(x) != "")
  
  # extract tag keys and tag values
  tag_keys <- names(xts::xtsAttributes(x)[valid_attr])
  tag_values <- xts::xtsAttributes(x)[valid_attr]
  
  # handle commas and spaces in values
  tag_values <- gsub(pattern = "[ ]", replacement = "\\\\ ", x = tag_values)
  tag_values <- gsub(pattern = "[,]", replacement = "\\\\,", x = tag_values)
  tag_values <- gsub(pattern = "[=]", replacement = "\\\\=", x = tag_values)
  
  # handle empty values in keys
  tag_values <- gsub(pattern = "numeric\\(0\\)|character\\(0\\)",
                     replacement = "NA",
                     x = tag_values)
  
  # merge tag keys and values
  tag_key_value <-
    paste(tag_keys, tag_values, sep = "=", collapse = ",")
  
  # create time vector
  time <- format(as.numeric(zoo::index(x)) * get_precision_divisor(precision),
                 scientific = FALSE)
  
  # default NA string 
  na_string <- "NA"
  
  # make sure all integers end with "i", this also sets mode to "character"
  # s. https://github.com/influxdb/influxdb/issues/3519
  if ((use_integers == TRUE) & is.numeric(x)) {
    if (all(x == floor(x), na.rm = TRUE)) {
      x[, ] <- sapply(seq_len(ncol(x)), function(y) paste(x[, y], "i", sep = ""))
      # define na_string to substitute later
      na_string <- "NAi"
    }
    
  } else {
    if (!is.numeric(x)) {
      # add quotes if matrix contains no numerics i.e. -> characters
      options("useFancyQuotes" = FALSE)
      x[, ] <- sapply(seq_len(ncol(x)), function(y) base::dQuote(x[, y]))
      # trim leading and trailing whitespaces
      x <- gsub("^\\s+|\\s+$", "", x)
      # define na_string to substitute later
      na_string <- paste0("\"", "NA", sep = "\"")
    }
    
  }
  
  # assign columnname to each element
  values <- sapply(seq_len(ncol(x)),
                   function(y)
                     paste(colnames(x)[y],
                           zoo::coredata(x)[, y],
                           sep = "="))
  
  # set R's NA values to a dummy string which can be removed easily
  # -> influxdb doesn't handle NA values
  # TODO: What if columnname contains "NA" ?
  values[grepl(na_string, values, fixed = TRUE)] <- "NA_to_remove"
  
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
  
  # invisibly return InfluxDB line protocol string
  invisible(influxdb_line_protocol)
  
}

#' @keywords internal
convert_to_line_protocol.data.frame <- function(x, 
                                                measurement,
                                                precision,
                                                use_integers = FALSE,
                                                ...,
                                                tag_cols = NULL,
                                                time_col = NULL) {
  
  # stop if data.frame provided contains NA's
  if (!all(!purrr::map_lgl(x, ~ any(is.na(.))))) {
    
    print(x %>% 
            purrr::map( ~ which(is.na(.), arr.ind = T)) %>% # transform to logical 
            purrr::keep( ~ length(.x) > 0)) # discard integer(0)
    
    stop("Handling of NA's in data.frames is currently not supported. 
         Rows containing NA's are given above!")  
    
  }
  
  # measurement
  tbl_measurement <- tibble::tibble(measurement = rep(measurement, 
                                                      times = nrow(x)))
  
  # TAG SET (are not necessary)
  if (!is.null(tag_cols)) {
    tbl_tags <- x %>%
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
  tbl_values <- x %>%
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
    tbl_time <- x %>% 
      # select time column if provided 
      dplyr::select(dplyr::one_of(time_col)) %>% 
      # rename for easier coding
      dplyr::rename(time = !!time_col) %>% 
      dplyr::mutate(time = format(as.numeric(time) * get_precision_divisor(precision),
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