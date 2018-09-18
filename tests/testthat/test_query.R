
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
  
  data2a <- influx_select(con, "stbmod", 
                          field_keys = "value", 
                          where = "Ort ='Flachbau'",
                          measurement = "MengeNEZ",
                          limit = 10, 
                          return_xts = FALSE,
                          simplifyList = FALSE)
  
  data2b <- influx_select(con, "stbmod", 
                          field_keys = "value", 
                          where = "Ort ='Flachbau'",
                          measurement = "MengeNEZ",
                          limit = 10, 
                          return_xts = FALSE,
                          simplifyList = TRUE)
  
  data2c <- influx_select(con, "stbmod", 
                          field_keys = "value", 
                          where = "Ort ='Flachbau'",
                          measurement = "MengeNEZ",
                          group_by = "*",
                          limit = 10, 
                          return_xts = FALSE,
                          simplifyList = FALSE)
  
  data2d <- influx_select(con, "stbmod", 
                          field_keys = "value", 
                          where = "Ort ='Flachbau'",
                          measurement = "MengeNEZ",
                          group_by = "*",
                          limit = 10, 
                          return_xts = FALSE,
                          simplifyList = TRUE)
  
  testthat::expect_is(data1a, class = "list")
  testthat::expect_is(data1b, class = "list")
  testthat::expect_equal(data1c, data1d[[1]][[1]])
  testthat::expect_equal(data2a[[1]]$time, data2b$time) # series_tags are empty -> error
  testthat::expect_equal(data2c[[1]], data2d)
  
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

testthat::test_that("multiple query with chunking and xts result", { 
  
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

testthat::test_that("empty results", { 
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  empty_results <- influx_query(con = con,
                                db =  "stbmod",
                                query = "select * from MengeNEZ where Ort='idontknow';
                                         select * from MengeNEZ where Ort='idontknoweither'", 
                                return_xts = FALSE,
                                simplifyList = TRUE)
  
  testthat::expect_true(length(empty_results) == 2 && all(sapply(empty_results, is.null)))
  
})

testthat::test_that("empty and non-empty results ", { 
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  empty_results <- influx_query(con = con,
                                db =  "stbmod",
                                query = "select * from MengeNEZ where Ort='idontknow';
                                         select value from MengeNEZ where Ort='Flachbau' limit 10;
                                         select * from MengeNEZ where Ort='idontknoweither'", 
                                return_xts = FALSE,
                                simplifyList = FALSE)
  
  testthat::expect_true(length(empty_results) == 3 && 
                          ncol(empty_results[[2]]) == 6 && 
                          nrow(empty_results[[2]]) == 10)
  
  
})