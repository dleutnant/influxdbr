
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
  tmp <- xts::xts(x = runif(1e1),
                  order.by = seq(as.POSIXct("1970-1-1"), by = "hours", length.out = 1e1))
  colnames(tmp) <- "one"
  # add second column
  tmp <- cbind(tmp, tmp)
  colnames(tmp) <- c("one", "two")
  
  # numeric matrix
  tmp[8,1] <- NA
  tmp[3,2] <- NA
  influx_write(con = con, db = "test", x = tmp, measurement = "test_num")
  influx_query(con = con, db = "test", query = "SELECT * from test_num")
  
  # integer matrix
  tmp[,1] <- 1:10
  tmp[,2] <- 20:21
  tmp[8,1] <- NA
  tmp[3,2] <- NA
  influx_write(con = con, db = "test", x = tmp, measurement = "test_int", use_integers = T)
  influx_query(con = con, db = "test", query = "SELECT * from test_int")
  
  # character matrix
  tmp[,1] <- paste0("NAME", 1:10)
  tmp[,2] <- paste0("NAME_TWO_", 1:10)
  tmp[8,1] <- NA
  tmp[3,2] <- NA
  influx_write(con = con, db = "test", x = tmp, measurement = "test_str")
  influx_query(con = con, db = "test", query = "SELECT * from test_str")
  
  # mixed data.frame
  tmp_length <- 1e2
  tmp1 <- xts::xts(x = 1:tmp_length, 
                   order.by =  seq(as.POSIXct("1970-1-1"), by = "mins", length.out = tmp_length))
  tmp2 <- xts::xts(x = paste0("NAME_TWO_", 1:tmp_length),
                   order.by =  zoo::index(tmp1))
  tmp1[8] <- NA
  tmp2[3] <- NA
  colnames(tmp1) <- c("one")
  colnames(tmp2) <- c("two")
  system.time(influx_write(con = con, db = "test", x = tmp1, measurement = "test_df"))
  system.time(influx_write(con = con, db = "test", x = tmp2, measurement = "test_df"))
  system.time(tmp <- influx_query(con = con, db = "test", query = "SELECT * from test_df limit 1000"))
  
  # delete measurements
  purrr::walk(paste("test", c("num", "int", "str", "df"), sep = "_"), ~ drop_measurement(con, "test", .))

})

testthat::test_that("write xts with sub-second accuracy", { 
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  # how many digits to print?
  options(digits.secs = 3)
  
  tmp <- xts::xts(runif(10), order.by = runif(10) + Sys.time())
  colnames(tmp) <- c("accuracy")
  influx_write(con = con, db = "test", x = tmp, measurement = "subsecond_acc", precision = "ms")
  tmp_subsecond <- influx_query(con = con, db = "test", query = "SELECT * from subsecond_acc ORDER BY time DESC LIMIT 10")

  testthat::expect_is(object = tmp_subsecond, class = "list")
  
  # delete measurement
  drop_measurement(con, "test", "subsecond_acc")

})
  
testthat::test_that("valid data.frame", {
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  library(magrittr)
  
  df <- tibble::tibble(time = seq(from = Sys.time(), by = "-5 min", length.out = 10),
                       `tag_one,` = sample(LETTERS[1:5], 10, replace = T),
                       tag_two = sample(LETTERS[1:5], 10, replace = T),
                       `tag three` = sample(paste(LETTERS[1:5], "tag"), 10, replace = T),
                       field_chr = sample(paste(" ", LETTERS[1:5], "field"), 10, replace = T),
                       field_float = stats::runif(10),
                       field_int = sample(1:100000000,10),
                       field_bool = stats::runif(10) > 0.5) %>%
    dplyr::arrange(time)
  
  # NAs not supported!
  # df[c(3, 8), c(5)] <- NA
  # df[c(4), c(6)] <- NA
  # df[c(2), c(7)] <- NA
  # df[c(4,6), c(7)] <- NA
  
  influxdbr:::convert_to_line_protocol.data.frame(x = df,
                                       measurement = "test",
                                       #tag_cols = c("tag_one,", "tag_two", "tag three"),
                                       time_col = "time",
                                       precision = "s",
                                       use_integers = FALSE) %>% cat
  
  # write full df
  influxdbr::influx_write(x = df, 
                          con = con,
                          db = "test",
                          measurement = "new_df2",
                          time_col = "time",
                          tag_cols = c("tag_one,", "tag_two", "tag three"),
                          max_points = 2,
                          use_integers = TRUE)
  
  # write df without tags
  influxdbr::influx_write(x = df, 
                          con = con,
                          db = "test",
                          measurement = "new_df3",
                          time_col = "time",
                          use_integers = TRUE)
  
  # write df without time
  influxdbr::influx_write(x = df %>% 
                            dplyr::mutate(time_chr = as.character(time)) %>% 
                            dplyr::select(-time), 
                          con = con,
                          db = "test",
                          measurement = "new_df4",
                          tag_cols = c("tag_one,", "tag_two", "tag three"),
                          use_integers = TRUE)
  
  # write only data
  influxdbr::influx_write(x = df %>% 
                            dplyr::mutate(time_chr = as.character(time)) %>% 
                            dplyr::select(-time), 
                          con = con,
                          db = "test",
                          # if no time and no tags are supplied, only one point is written
                          # remember: points which don't have timestamps will get the same 
                          # timestamp for the batch
                          # therefore set at least a unique dummy tag:
                          tag_cols = "time_chr", 
                          measurement = "new_df5",
                          precision = "ns",
                          points = 1,
                          use_integers = TRUE)
  
  # query the data to check once again
  tmp_df_full <-
    influxdbr::influx_query(
      con = con,
      db = "test",
      query = "select * from new_df2 group by *",
      return_xts = FALSE
    )
  tmp_df_no_tags <-
    influxdbr::influx_query(
      con = con,
      db = "test",
      query = "select * from new_df3 group by *",
      return_xts = FALSE
    )
  tmp_df_no_time <-
    influxdbr::influx_query(
      con = con,
      db = "test",
      query = "select * from new_df4 group by *",
      return_xts = FALSE,
      timestamp_format = "ms"
    )
  tmp_df_data_only <-
    influxdbr::influx_query(
      con = con,
      db = "test",
      query = "select * from new_df5 group by *",
      return_xts = FALSE,
      timestamp_format = "ms"
    )
  tmp_xts <-
    influxdbr::influx_query(
      con = con,
      db = "test",
      query = "select * from new_df2 group by *",
      return_xts = TRUE
    )
  
  # delete measurements
  purrr::walk(paste0("new_df", 2:5), ~ drop_measurement(con, "test", .))
  
})
  