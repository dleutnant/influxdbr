drop_measurement <- function(con, db, measurement) {

  result <- influxdbr::influx_query(con = con,
                                    db = db,
                                    query = paste("DROP MEASUREMENT",
                                                  measurement),
                                    return_xts = F)
  return(result)

}

drop_series <- function(con, db, id=NULL, from=NULL, where=NULL) {

  query <- "DROP SERIES"

  if (!is.null(id)) {

    query <- paste(query, id)

  } else {

    query <- ifelse(is.null(from),
                    query,
                    paste(query, "FROM", from))

    query <- ifelse(is.null(where),
                    query,
                    paste(query, "WHERE", where))

  }

  result <- influxdbr::influx_query(con = con,
                                    db = db,
                                    query = query,
                                    return_xts = F)

  return(result)

}

drop_database <- function(con, db) {

  result <- influxdbr::influx_query(con = con,
                                    db = db,
                                    query = paste("DROP DATABASE",
                                                  db),
                                    return_xts = F)
  return(result)

}

influx_select <- function(con,
                          db,
                          value,
                          rp = NULL,
                          from,
                          where = NULL,
                          group_by = NULL,
                          limit = NULL,
                          offset = NULL,
                          return_xts = TRUE,
                          verbose = FALSE ) {

  if (!is.null(rp)) {
    options("useFancyQuotes" = FALSE)
    from <- paste(base::dQuote(rp), from, sep = ".")
  }

  query <- paste("SELECT", value, "FROM", from)

  query <- ifelse(is.null(where),
                  query,
                  paste(query, "WHERE", where))

  query <- ifelse(is.null(group_by),
                  paste(query, "GROUP BY *"),
                  paste(query, "GROUP BY", group_by))

  query <- ifelse(is.null(limit),
                  query,
                  paste(query, "limit", limit))

  query <- ifelse(is.null(offset),
                  query,
                  paste(query, "OFFSET", offset))


  result <- influxdbr::influx_query(con = con,
                                    db = db,
                                    query = query,
                                    return_xts = return_xts,
                                    verbose = verbose)

  invisible(result)

}
