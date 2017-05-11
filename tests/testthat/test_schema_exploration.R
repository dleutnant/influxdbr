library(magrittr)

testthat::context("schema exploration")

testthat::test_that("connection", {
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()

  # setup influx connection  
  con <<- influx_connection(group = "admin")
  
  testthat::expect_is(object = con, class = "list")
  
})


testthat::test_that("show commands", { 
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  testthat::expect_is(show_databases(con), class = "tbl_df")
  
  testthat::expect_is(show_measurements(con, db = "stbmod"), class = "tbl_df")
  
  testthat::expect_is(show_series(con, db = "stbmod"), class = "tbl_df")
  
  testthat::expect_is(show_tag_keys(con, db = "stbmod"), class = "tbl_df")
  
  testthat::expect_is(show_tag_values(con, db = "stbmod", key = "Ort"), class = "tbl_df")
  
  testthat::expect_is(show_field_keys(con, db = "stbmod"), class = "tbl_df")
  
  testthat::expect_is(show_retention_policies(con, db = "stbmod"), class = "tbl_df")
  
})

