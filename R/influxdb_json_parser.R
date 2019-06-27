### rbind_list machinery

base_rbind_fallback <- function(lst) {
  do.call(rbind.data.frame, c(lst, stringsAsFactors = FALSE, make.row.names = FALSE))
}

dplyr_rbind_fallback <- function(lst) {
  # dplyr requires names arguments; add dummies
  if (is.null(names(lst[[1]]))) {
    dummy_names <- as.character(seq_along(lst[[1]]))
    lst <- lapply(lst, function(l) {names(l) <- dummy_names; l})
  }
  dplyr::bind_rows(lst)
}

rbind_list <- local({
  rl <- NULL
  function(lst) {
    if (is.null(rl)) {
      rl <<-
        switch(getOption("influxdbr.rbind.backend", "auto"),
               data.table = data.table::rbindlist,
               dplyr = dplyr_rbind_fallback,
               base = base_rbind_fallback,
               if (requireNamespace("data.table", quietly = TRUE)) {
                 data.table::rbindlist
               } else if (requireNamespace("dplyr", quietly = TRUE)) {
                 dplyr_rbind_fallback
               } else {
                 base_rbind_fallback
               })
    }
    rl(lst)
  }
})

## environment(rbind_list)[["rl"]] <- data.table::rbindlist
## environment(rbind_list)[["rl"]] <- dplyr_rbind_fallback
## environment(rbind_list)[["rl"]] <- base_rbind_fallback

series_to_df <- function(x) {
  if (!is.null(x)) {
    # Names are not uniquified, so user will be OK even if there is a collision.
    out <- c(measurement = x$name,
             if (!is.null(x$columns)) {
               colnames <- unlist(x$columns, FALSE, FALSE)
               if (is.null(x$values)) {
                 ## corner case of null values (e.g. show_users)
                 return(as.data.frame(matrix(nrow = 0, ncol = length(colnames), dimnames = list(NULL, colnames))))
               } else {
                 out <- rbind_list(.Call(`C_fill_nulls`, x$values))
                 colnames(out) <- colnames
                 out
               }
             },
             x$tags,
             list(check.names = FALSE, 
                  stringsAsFactors = FALSE))
    do.call(data.frame, out)
  }
}

json_to_df1 <- function(x) {
  if (!is.null(x$error))
    stop(sprintf("influx error: %s", x$error))
  out <- rbind_list(lapply(x$series, series_to_df))
  attr(out, "statement_id") <- x$statement_id
  attr(out, "tags") <- x$series[[1]]$tags
  out
}

json_to_df <- function(x, time_format, tags_as_factors) {
  out <-
    unlist(x, recursive = FALSE, use.names = FALSE) %>% 
    lapply(json_to_df1)
  sids <- vapply(out, attr, 0L, "statement_id")
  if (any(sids > 0)) {
    ## multi-query
    splits <- split(seq_along(sids), sids)
    lapply(seq_along(splits),
           function(i) {
             bind_homogenuos_dfs(
               out[splits[[i]]], time_format, tags_as_factors)
           })
  } else {
    bind_homogenuos_dfs(out, time_format, tags_as_factors)
  }
}


bind_homogenuos_dfs <- function(dfs, time_format, tags_as_factors) {
  out <- rbind_list(dfs)
  ## Return NULL on empty output. For queries with columns but no values
  ## (e.g. show_users) nrow can be 0 with non 0 columns.
  if (ncol(out) == 0)
    return(NULL)
  ## convert known tags to factors
  if (tags_as_factors) {
    tags <- attr(dfs[[1]], "tags")
    if (!is.null(tags)) {
      for (nm in names(tags))
        out[[nm]] <- as.factor(out[[nm]])
    }
  }
  ## schema exploration response don't have time column
  if (!is.null(out$time)) {
    out$time <-
      .POSIXct(out$time/precision_divisor(time_format), tz = "UTC")
    ## make time first for the sake of as.zoo/xts
    out <- out[c("time", setdiff(names(out), "time"))]
  }
  class(out) <- c("influxdbr.response", "data.frame")
  out
}

# Return a 3 level value irrespective of `chunked` option:
# List of 3                  ; <- our paging
# $ 1 :List of 3             ; <- chunks within a page
#  ..$ :List of 3            ; <- series within a chunk
#   .. ..$ statement_id: int 0
#   .. ..$ series      :List of 1
#   .. .. ..$ :List of 4
#   .. .. .. ..$ name   : chr "A"
#   .. .. .. ..$ tags   :List of 1
#   .. .. .. .. ..$ b: chr "a"
#   .. .. .. ..$ columns:List of 3
#   .. .. .. .. ..$ : chr "time"
#   .. .. .. .. ..$ : chr "a"
#   .. .. .. .. ..$ : chr "c"
#   .. .. .. ..$ values :List of 1
#   .. .. .. .. ..$ :List of 3
#   .. .. .. .. .. ..$ : num 1.56e+18
#   .. .. .. .. .. ..$ : int 1
#   .. .. .. .. .. ..$ : int 1
#   .. ..$ partial     : logi TRUE
#   ..$ :List of 3
#   .. ..$ statement_id: int 0
#   .. ..$ series      :List of 1
#   .. .. ..$ :List of 4
#   .. .. .. ..$ name   : chr "A"
#   .. .. .. ..$ tags   :List of 1
#   .. .. .. .. ..$ b: chr "b"
#   .. .. .. ..$ columns:List of 3
#' @keywords internal
parse_response <- function(raw, chunked = TRUE, handler = NULL) {
  # chunked = TRUE works even when there is one chunk, but it's somewhat slower
  con <- rawConnection(raw, "rb")
  on.exit(close(con))

  if (chunked) {

    env <- NULL
    if (is.null(handler)) {
      env <- new.env()
      i <- 0
      handler <- function(out) {
          i <<- i + 1
          assign(as.character(i), out, envir = env)
        }
    }

    pagesize <- 100
    repeat {
      pages <- readLines(con, n = pagesize, encoding = "UTF-8")
      if (length(pages) > 0) {
        handler(lapply(pages[nzchar(pages)],
                       function(el) jsonlite::parse_json(el)[[1]][[1]]))
      }
      if(length(pages) < pagesize)
        break
    }
    if (!is.null(env) && length(env) > 0) {
      as.list(env)
    }
    
  } else {
    jsonlite::parse_json(con)
  }
}

