
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
  
  
  df <- data.frame(measurement = "test",
                   time = seq(from = Sys.time(), by = "-5 min", length.out = 10),
                   tag_one = sample(LETTERS, 10, replace = F),
                   tag_two = sample(LETTERS, 10, replace = F),
                   field_chr = sample(LETTERS, 10, replace = F), 
                   field_float = stats::runif(10), 
                   field_bool = stats::runif(10) > 0.5)
  
})