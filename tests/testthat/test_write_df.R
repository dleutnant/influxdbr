
testthat::context("testing influx_write")

# setup influx connection

testthat::test_that("connection", {
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  con <<- influx_connection(group = "admin")
  
  testthat::expect_is(object = con, class = "list")
  
})

testthat::test_that("valid data.frame", {
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()

  df <- tibble::tibble(time = seq(from = Sys.time(), by = "-5 min", length.out = 10),
                       `tag_one,` = sample(LETTERS, 10, replace = F),
                       tag_two = sample(LETTERS, 10, replace = F),
                       field_chr = sample(LETTERS, 10, replace = F),
                       field_float = stats::runif(10),
                       field_int = sample(1:100000000,10),
                       field_bool = stats::runif(10) > 0.5) %>%
    dplyr::arrange(time)

  # NAs not supported!
  # df[c(3, 8), c(5)] <- NA
  # df[c(4), c(6)] <- NA
  # df[c(2), c(7)] <- NA
  # df[c(4,6), c(7)] <- NA

  # .df_to_influxdb_line_protocol(df,
  #                               measurement = "test",
  #                               tag_cols = paste("tag", c("one,", "two"), sep = "_"),
  #                               time_col = NULL,
  #                               precision = "s",
  #                               use_integers = F) %>% cat

  influxdbr:::influx_write_df(con = con,
                  db = "new_df",
                  measurement = "new_df2",
                  x = df,
                  time_col = "time",
                  tag_cols = paste("tag", c("one,", "two"), sep = "_"),
                  max_points = 2,
                  use_integers = TRUE)

  tmp <- influxdbr::influx_query(con = con, db = "new_df", query = "select * from new_df2", return_xts = F)

})