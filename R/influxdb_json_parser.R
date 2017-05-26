#' @keywords internal
query_list_to_tibble <- function(x, timestamp_format) {
  
  #x <- debug_data %>% purrr::map(response_to_list)
  # stop()
  
  # create divisor for different timestamp format
  div <- switch(timestamp_format,
                "n" = 1e+9,
                "u" = 1e+6,
                "ms" = 1e+3,
                "s" = 1,
                "m" = 1 / 60,
                "h" = 1 / (60 * 60)
  )
  
  # set default result
  result_na <- tibble::tibble(statement_id = NA,
                              series_names = NA,
                              series_tags = NA,
                              series_values = NA,
                              series_partial = NA)
  
  # remove hierarchies to get direct access results level
  while (!("results" %in% names(x))) {
    x <- purrr::flatten(x)
  }
  
  # flatten once again to get directly to "statement_id" and "series"
  x <- purrr::flatten(x)
  
  # iterate over result array
  list_of_result <- purrr::map(x, .f = function(series_ele) {
    
    # set dummy result
    result <- result_na
    
    if (!is.null(series_ele$statement_id)) {
      # extract "statement_id"
      statement_id <- series_ele$statement_id
    }
      
    if (!is.null(series_ele$series)) {
      # extract "measurement names"
      series_names <- purrr::map(series_ele$series, "name") %>%
        unlist() %>%
        `if`(is.null(.), NA, .)
        
      # extract "tags"
      series_tags <- purrr::map(series_ele$series, "tags") %>%
        purrr::map(tibble::as_tibble)
        
      # extract "columns"
      series_columns <- purrr::map(series_ele$series, "columns") %>%
        purrr::map(unlist)
        
      # extract values
      # edit 26/05/2017: code is less efficient but more stable
      series_values <- purrr::map(series_ele$series, "values") %>%
        # set names 
        purrr::map2(., .y  = series_columns, ~ purrr::map(., 
                                                          purrr::set_names, 
                                                          nm = .y)) %>%
        # convert to tibble (time consuming! alternative?!)
        purrr::map( ~ purrr::map_df(., tibble::as.tibble)) %>% 
        # convert int to dbl (reuqired for unnesting)
        purrr::map( ~ purrr::map_if(., is.integer, as.double)) %>% 
        # influxdb ALWAYS stores data in GMT!!
        purrr::map( ~ purrr::map_at(., .at = "time",
                                    ~ as.POSIXct(. / div, 
                                                 origin = "1970-1-1",
                                                 tz = "GMT")) %>%
                      tibble::as_tibble(.))

      # is partial?
      series_partial <-
        ifelse(is.null(series_ele$partial), FALSE, TRUE)
        
      # RETURN AGGREGATED TIBBLE WITH LIST COLUMNS!
      result <- tibble::tibble(statement_id,
                               series_names,
                               series_tags,
                               series_values,
                               series_partial)
        
      # unnest list-columns if content is present (here: tags)
      series_tags_rows <- purrr::map_int(result$series_tags, nrow)
      
      if (all(series_tags_rows != 0)) {
        result <- tidyr::unnest(result, series_tags, .drop = FALSE)
      }
      
      # unnest list-columns if content is present (here: values)
      series_values_rows <- purrr::map_int(result$series_values, nrow)

      if (all(series_values_rows != 0))  {
        result <- tidyr::unnest(result, series_values, .drop = FALSE)
      }
      
    } else {
      # in case of an error...
      if (!is.null(series_ele$error)) {
        stop(series_ele$error, call. = FALSE)
      }
    
      warning("no series returned")
      
    }
      
    return(result)
      
  })
  
  ### in case of CHUNKED responses, concatenate tables with same statement_id
  list_of_result <- list_of_result %>% # take the list of results
    purrr::map("statement_id") %>% # extract "statement_id" of each result
    purrr::map_int(unique) %>% # create a vector with unique "statement_id"
    rle %>% # perform run length encoding to get the length of each "statement_id"
    rle_seq_to_list %>% # own function to make a list of sequences from rle
    purrr::map( ~ dplyr::bind_rows(list_of_result[.])) # rbind results
    
  # return list of tibbles
  return(list_of_result)
  
}

#' @keywords internal
post_list_to_tibble <- function(x) {
  #x <- debug_data
  
  # remove hierarchies:
  # flatten list to get direct access to list of results
  while (!("results" %in% names(x))) {
    x <- x %>% purrr::flatten(.)
  }
  
  # flatten once again to get directly to "statement_id" and series
  x <- x %>% purrr::flatten(.)
  
  # create tibbles of results
  result_tbl <- purrr::map_df(x, tibble::as_tibble)
  
  # return ONE tibble
  return(result_tbl)
}

#' @keywords internal
response_to_list <- function(x) {
  # did we receive chunked results?
  chunked <- grepl("partial", x, fixed = T)
  
  # if we've got chunked data, we need to split them by 'newlines', i.e. "\n"
  if (chunked) {
    # split chunked response
    x <- base::strsplit(x, split = "\n") %>%
      purrr::flatten(.)
  }
  
  # transform json to R's list data type
  response_data <- purrr::map(x, 
                              jsonlite::fromJSON,      
                              simplifyVector = FALSE,
                              simplifyDataFrame = FALSE,
                              simplifyMatrix = FALSE)
  
  # return transformed data as R list
  return(response_data)
  
}

#' @keywords internal
rle_seq_to_list <- function(x) {
  # prepare function output: list of sequences
  idx_lst <- vector(mode = "list", length = length(x$lengths))
  
  # add starting 0
  x <- c(0, cumsum(x$lengths))
  
  # map instead of for loop ?? (no speed gain expected, only for consistency)
  for (i in seq_len(length(idx_lst))) {
    # create the sequences
    idx_lst[[i]] <- seq(x[i] + 1, x[i + 1])
    
  }
  # return index list: e.g.: [[1]] 1 2 3 4 [[2]] 5 6 7 8
  return(idx_lst)
}

#' @keywords internal
tibble_to_xts <- function(x) {
  # x <- list_of_result[[1]]
  #
  # a tibble header looks like:
  # # A tibble: 60,804 × 29
  #    statement_id series_names series_partial   [tags]     time [fields]
  # *         <int>        <chr>          <lgl>      ...   <dttm>
  
  # assign group identifier for all columns but time and field
  list_of_tibble <- x %>%
    # remove series_tags in case of zero tags
    `if`("series_tags" %in% colnames(.),
         dplyr::select(.,-series_tags),
         .) %>%
    # select all "tagkey" columns
    dplyr::select(statement_id:time,-time) %>%
    # create a group index
    dplyr::group_indices_(.dots = colnames(.)) %>%
    # add group index to tibble
    dplyr::mutate(x, grp_idx = .) %>%
    # create list of tibble by group
    split(., .[["grp_idx"]]) %>%
    # remove index
    purrr::map( ~ dplyr::select(.,-grp_idx))
  
  # extract unique tag sets
  list_of_tags <- list_of_tibble %>%
    purrr::map( ~ dplyr::select(., statement_id:time, -time) %>%
                  dplyr::distinct() %>% 
                  as.list)
  
  # create list_of_values from each "field" column
  # hint: We create an xts object for each column to preserve the column type.
  # An xts object is based on a matrix and thus can store one type only.
  list_of_values <- list_of_tibble %>%
    purrr::map( ~ dplyr::select(.,-(statement_id:time)) %>% 
                  as.list)
  
  # create list_of_times
  list_of_times <- list_of_tibble %>%
    purrr::map( ~ dplyr::select(., time) %>% 
                  as.list)
  
  # create list of xts
  list_of_xts <- list(list_of_values, list_of_times, list_of_tags) %>%
    purrr::pmap(function(x, y, z) {
      purrr::map2(.x = x, .y = names(x), function(a, b) {
        ts <- xts::xts(a, order.by = y[[1]], tzone = "GMT") # influx always returns GMT
        colnames(ts) <- b # rename column according to field value
        return(ts) 
        }) %>%
        purrr::map(., function(c) {
          xts::xtsAttributes(c) <- purrr::flatten(z)
          return(c)
        })
    }) %>%
    purrr::flatten(.)
  
  # rename list elements according to series name
  names(list_of_xts) <-
    list_of_tags %>% 
    purrr::map_chr("series_names") %>% 
    purrr::map2(., .y = purrr::map_int(list_of_values, length), 
                    ~ rep(.x, each = .y)) %>% 
    purrr::flatten_chr(.)
  
  return(list_of_xts)
  
}
