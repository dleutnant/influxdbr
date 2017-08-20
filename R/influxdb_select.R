#' @title influx select helper
#' @description This function is a convenient wrapper for selecting data from a measurement
#' by calling \code{influx_query} with the corresponding query.
#' @inheritParams influx_query
#' @param field_keys Specifies the fields to be selected.
#' @param rp The name of the retention policy.
#' @param measurement Sets the name of the measurement.
#' @param where Apply filter on tag key values.
#' @param group_by The group_by clause in InfluxDB is used not only for
#' grouping by given values, but also for grouping by given time buckets.
#' @param limit Limits the number of the n oldest points to be returned.
#' @param slimit logical. Sets limiting procedure (slimit vs. limit).
#' @param offset Offsets the returned points by the value provided.
#' @param order_desc logical. Change sort order to descending.
#' @param return_xts logical. Sets the return type. If set to TRUE, a list of xts objects
#' is returned, FALSE gives list of tibbbles.
#'
#' @return A list of xts or tibbles.
#' @export
#' @references \url{https://docs.influxdata.com/influxdb/}
influx_select <- function(con,
                          db,
                          field_keys,
                          rp = NULL,
                          measurement,
                          where = NULL,
                          group_by = NULL,
                          limit = NULL,
                          slimit = FALSE,
                          offset = NULL,
                          order_desc = FALSE,
                          return_xts = TRUE, 
                          simplifyList = FALSE) {
  if (!is.null(rp)) {
    options("useFancyQuotes" = FALSE)
    measurement <- paste(base::dQuote(rp), measurement, sep = ".")
  }
  
  query <- paste("SELECT", field_keys, "FROM", measurement)
  
  query <- ifelse(is.null(where),
                  query,
                  paste(query, "WHERE", where))
  
  query <- ifelse(is.null(group_by),
                  query,
                  paste(query, "GROUP BY", group_by))
  
  query <- ifelse(!order_desc,
                  query,
                  paste(query, "ORDER BY time DESC"))
  
  query <- ifelse(is.null(limit),
                  query,
                  paste(query, ifelse(slimit, "SLIMIT", "LIMIT"), 
                        format(as.integer(limit), scientific = FALSE)))
  
  query <- ifelse(is.null(offset),
                  query,
                  paste(query, "OFFSET", 
                        format(as.integer(offset), scientific = FALSE)))
  
  result <- influx_query(
    con = con,
    db = db,
    query = query,
    return_xts = return_xts, 
    simplifyList = simplifyList
  )
  
  if (is.null(result))
    return(NULL)
  
  invisible(result)
  
}
