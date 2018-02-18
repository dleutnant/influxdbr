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
            p = con$pass)
  
  # add query
  q <- c(q, q = query)
  
  # submit POST
  response <- httr_POST(con = con, query = q, endpoint = "query")
  
  # if curl fails return NULL
  if (is.null(response)) {
    return(NULL)
  }
  
  # Check for communication errors
  check_srv_comm(response)
  
  response <- httr::content(response, "text", encoding = "UTF-8") %>%  # convert to chars
    purrr::map(response_to_list) %>%
    purrr::map_df(post_list_to_tibble)
  
  # if everything is OK, there won't be columns such as "error" or "messages"..
  if (any(c("error", "messages") %in% colnames(response))) {
    # return result tbl visible
    return(response)
  }
  
  return(NULL)
}