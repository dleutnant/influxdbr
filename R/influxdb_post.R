#' @title send POST to an InfluxDB server
#' @description This function sends POST to an InfluxDB server. It is not 
#' exported and only used for some helper functions within this package.
#' @inheritParams influx_query
#' @return A tibble or NULL
#' @references \url{https://influxdb.com/}
#' @keywords internal
influx_post <- function(con,
                        db = NULL,
                        query = "") {
  
  # create query based on function parameters
  q <- list(db = db,
            u = con$user,
            p = con$pass,
            q = query)
    
  # submit POST
  response <- httr_POST(con = con, query = q, endpoint = "query")
  
  # if curl fails return NULL
  if (is.null(response)) {
    return(NULL)
  }
  
  check_response_errors(response)
  
  out <- parse_response(response$content, FALSE)

  # flatten list to get direct access to list of results
  while (!("results" %in% names(out))) {
    out <- unlist(out, FALSE, FALSE)
  }
  out <- unlist(out, FALSE)

  out[["query"]] <- query
  out[["status"]] <- httr::http_status(response)
  class(out) <- "invluxdbr.post.response"
  ## str(out)
  out
}
