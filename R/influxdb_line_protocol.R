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

  # handle special characters
  measurement <- replace_spec_char(measurement, do_equal = FALSE)
  tag_keys <- replace_spec_char(tag_keys)
  tag_values <- replace_spec_char(tag_values)

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
      # check Option useFancyQuotes
      quotes <- getOption("useFancyQuotes")
      on.exit(options("useFancyQuotes" = quotes))
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
                                                measurement = NULL,
                                                precision = NULL,
                                                use_integers = FALSE,
                                                measurement_col = NULL,
                                                tag_cols = NULL,
                                                time_col = NULL,
                                                ...) {

  # stop if measurements contain NA's
  val_cols <- setdiff(colnames(x), c(tag_cols, time_col))
  for (nm in val_cols) {
    if (!(is.character(x[[nm]]) || is.factor(x[[nm]]))) {
      if (anyNA(x[[nm]])) {
        stop(sprintf("Handling of NA's in data.frames is currently not supported (col: '%s').", nm))
      }
    }
  }

  # MEASUREMENT
  the_measurement <-
    if (!is.null(measurement_col)) {
      validate_scalar_var(x, measurement_col)
      list(replace_spec_char(x[[measurement_col]], FALSE))
    } else if (!is.null(measurement)) {
      validate_scalar_var(x, measurement, FALSE)
      list(replace_spec_char(measurement, FALSE))
    } else {
      stop("`measurement` or `measurement_col` must be specified.")
    }

  # TAG SET (optional)
  the_tags <- list()
  for (nm in tag_cols) {
    the_tags <- append(the_tags,
                       list(",",
                            replace_spec_char(nm),
                            "=",
                            replace_spec_char(x[[nm]])))
  }

  # FIELD SET
  the_vals <- list()
  for (nm in val_cols) {
    the_vals <- append(the_vals,
                       list(",",
                            replace_spec_char(nm), # name
                            "=",
                            double_quote(x[[nm]]), # value
                            if(use_integers && is.integer(x[[nm]])) "i"))
  }
  if (length(val_cols) > 0)
    the_vals[[1]] <- " "

  # TIME (optional; defaults to the server time)
  the_time <- list()
  if (!is.null(time_col)) {
    validate_scalar_var(x, time_col)
    the_time <- list(" ", format_time(x[[time_col]], precision))
  }

  invisible(do.call(paste0, c(the_measurement, the_tags, the_vals, the_time)))

}

# method to convert the line protocol to a data.frame
# function is not exported
#' @keywords internal
line_protocol_to_array <- function(x) {

  # substitute [ ], [,] and [=]
  x <- gsub("\\ ", replacement = " ", x, fixed = TRUE)
  x <- gsub("\\,", replacement = ";;;ABC;;;", x, fixed = TRUE) # dummy
  x <- gsub("\\=", replacement = "=", x, fixed = TRUE)

  # split by ","
  splitted_string <- unlist(strsplit(x, split = ","))

  # subsitute dummy
  splitted_string <- gsub(pattern = ";;;ABC;;;", replacement = ",",
                          splitted_string, fixed = TRUE)

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

replace_spec_char <- function(x, do_equal = TRUE) {
  regexp <- if (do_equal) "([,= ])" else "([, ])"
  if (is.factor(x)) {
    # optimization for factors
    levels(x) <- gsub(regexp, "\\\\\\1", levels(x))
  } else if (is.character(x)) {
    x <- gsub(regexp, "\\\\\\1", x)
  }
  x
}

double_quote <- function(x) {
  if (is.factor(x)) {
    levels(x) <- paste0("\"", levels(x), "\"")
  } else if (is.character(x)) {
    x <- paste0("\"", x, "\"")
  }
  x
}

validate_scalar_var <- function(x, name, must_contain = TRUE) {
  var <- deparse(substitute(name))
  if (length(name) > 1)
    stop(sprintf("'%s' must be a scalar string.", var))
  if (must_contain && is.null(x[[name]]))
    stop(sprintf("'%s' is not in the data.frame (%s)", name, var))
}

format_time <- function(time, precision) {
  time <- as.numeric(time) * get_precision_divisor(precision)
  format(time, trim = TRUE, scientific = FALSE)
}
