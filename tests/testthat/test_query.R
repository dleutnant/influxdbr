
testthat::context("testing influx_query")

# setup influx connection

testthat::test_that("connection", {
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  con <<- influx_connection(group = "admin")
  
  testthat::expect_is(object = con, class = "list")
  
})

testthat::test_that("single query no chunking", { 
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  data1a <- influx_query(con = con,
                         chunked = FALSE,
                         db =  "stbmod",
                         timestamp_format = "n",
                         query = "select value from MengeNEZ where Ort='Flachbau' group by * limit 10", 
                         return_xts = FALSE) 
  
  data1b <- influx_select(con, "stbmod", 
                          field_keys = "value", 
                          where = "Ort ='Flachbau'",
                          measurement = "MengeNEZ",
                          group_by = "*", 
                          limit = 10)
  
  data1c <- influx_select(con, "stbmod", 
                          field_keys = "value", 
                          where = "Ort ='Flachbau'",
                          measurement = "MengeNEZ",
                          limit = 10, 
                          simplifyList = TRUE)
  
  data1d <- influx_select(con, "stbmod", 
                          field_keys = "value", 
                          where = "Ort ='Flachbau'",
                          measurement = "MengeNEZ",
                          limit = 10, 
                          simplifyList = FALSE)
  
  testthat::expect_is(data1a, class = "list")
  testthat::expect_is(data1b, class = "list")
  testthat::expect_equal(data1c, data1d[[1]][[1]])
  
})


testthat::test_that("multiple query no chunking", { 
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  data2 <- influx_query(con = con,
                        chunked = FALSE,
                        db =  "stbmod",
                        timestamp_format = "n",
                        query = "select value from MengeNEZ where Ort='Flachbau' group by * limit 10;
                                 select value from Durchfluss where Ort='Flachbau' limit 10", 
                        return_xts = FALSE) 
  
  testthat::expect_is(object = data2, class = "list")
  
})


testthat::test_that("single query with chunking", { 
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  data3 <- influx_query(con = con,
                        chunked = 10,
                        db =  "stbmod",
                        query = "select value from MengeNEZ, Durchfluss where Ort='Flachbau' group by * limit 100", 
                        return_xts = FALSE)
  
  testthat::expect_is(object = data3, class = "list")
  
})


testthat::test_that("multiple query with chunking", { 
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  data4 <- influx_query(con = con,
                        chunked = 10,
                        db =  "stbmod",
                        query = "select value from MengeNEZ where Ort='Flachbau' group by * limit 100;
                                 select value from Durchfluss where Ort='Flachbau' limit 100",
                        return_xts = FALSE)
  
  testthat::expect_is(object = data4, class = "list")
  
})

testthat::test_that("multiple query with chunking", { 
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  data5 <- influx_query(con = con,
                        chunked = 10,
                        db =  "stbmod",
                        query = "select value from MengeNEZ where Ort='Flachbau' group by * limit 100;
                                 select value from Durchfluss where Ort='Flachbau' limit 100",
                        return_xts = TRUE)
  
  testthat::expect_is(object = data5, class = "list")
  
})