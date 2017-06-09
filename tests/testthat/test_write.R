
testthat::context("testing influx_write")

# setup influx connection

testthat::test_that("connection", {
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  con <<- influx_connection(group = "admin")
  
  testthat::expect_is(object = con, class = "list")
  
})

testthat::test_that("write xts with NA", { 
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  # prepare tmp xts object
  tmp <- xts::xts(x = runif(10),
                  order.by = seq(as.POSIXct("1970-1-1"), by = "hours", length.out = 10))
  # add second column
  tmp <- cbind(tmp, tmp)
  colnames(tmp) <- c("one", "two")
  
  # numeric matrix
  tmp[8,1] <- NA
  tmp[3,2] <- NA
  influx_write(con = con, db = "tmp", xts = tmp, measurement = "test_num")
  influx_query(con = con, db = "tmp", query = "SELECT * from test_num")
  
  # integer matrix
  tmp[,1] <- 1:10
  tmp[,2] <- 20:21
  tmp[8,1] <- NA
  tmp[3,2] <- NA
  influx_write(con = con, db = "tmp", xts = tmp, measurement = "test_int", use_integers = T)
  influx_query(con = con, db = "tmp", query = "SELECT * from test_int")
  
  # character matrix
  tmp[,1] <- paste0("NAME", 1:10)
  tmp[,2] <- paste0("NAME_TWO_", 1:10)
  tmp[8,1] <- NA
  tmp[3,2] <- NA
  influx_write(con = con, db = "tmp", xts = tmp, measurement = "test_str")
  influx_query(con = con, db = "tmp", query = "SELECT * from test_str")
  
  # mixed data.frame
  tmp1 <- xts::xts(x = 1:1000, 
                   order.by =  seq(as.POSIXct("1970-1-1"), by = "hours", length.out = 1000))
  tmp2 <- xts::xts(x = paste0("NAME_TWO_", 1:1000),
                   order.by =  seq(as.POSIXct("1970-1-1"), by = "hours", length.out = 1000))
  tmp1[8] <- NA
  tmp2[3] <- NA
  colnames(tmp1) <- c("one")
  colnames(tmp2) <- c("two")
  influx_write(con = con, db = "tmp", xts = tmp1, measurement = "test_df")
  influx_write(con = con, db = "tmp", xts = tmp2, measurement = "test_df")
  influx_query(con = con, db = "tmp", query = "SELECT * from test_df limit 1000")
  
})
  
  