testthat::context("testing influx_write")

## testthat currently fails badly on tibbles
DF <- as.data.frame

## setup df

time0 <- .POSIXct(0, tz = "UTC")
set.seed(100)
df <- tibble::tibble(time = seq(from = time0, by = "-5 min", length.out = 10),
                     `tag_one,` = sample(LETTERS[1:5], 10, replace = T),
                     tag_two = sample(LETTERS[1:5], 10, replace = T),
                     `tag three` = sample(paste(LETTERS[1:5], "tag"), 10, replace = T),
                     field_chr = sample(paste(" ", LETTERS[1:5], "field"), 10, replace = T),
                     field_float = stats::runif(10),
                     field_int = sample(1:100000000,10),
                     field_bool = stats::runif(10) > 0.5) %>% dplyr::arrange(time)
df[1:3, "tag_two"] <- c("B,C", "B C", "B=C")
df[1:4, "field_chr"] <- c(" B,C", " B C", " B=C", " \"D\" ")

# NAs not supported!
# df[c(3, 8), c(5)] <- NA
# df[c(4), c(6)] <- NA
# df[c(2), c(7)] <- NA
# df[c(4,6), c(7)] <- NA


# setup influx connection
testthat::test_that("connection", {

  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()

  con <<- influx_connection(group = "admin")
  drop_database(con, "test")
  create_database(con, "test")

  testthat::expect_is(object = con, class = "list")

})

## testthat::test_that("write xts with NA", {

##   # only local tests
##   testthat::skip_on_cran()
##   testthat::skip_on_travis()

##   # prepare tmp xts object
##   tmp <- xts::xts(x = runif(1e1),
##                   order.by = seq(as.POSIXct("1970-1-1"), by = "hours", length.out = 1e1))
##   colnames(tmp) <- "one"
##   # add second column
##   tmp <- cbind(tmp, tmp)
##   colnames(tmp) <- c("one", "two")

##   # numeric matrix
##   tmp[8,1] <- NA
##   tmp[3,2] <- NA
##   influx_write(con = con, db = "test", x = tmp, measurement = "test_num")
##   influx_query(con = con, db = "test", query = "SELECT * from test_num")

##   # integer matrix
##   tmp[,1] <- 1:10
##   tmp[,2] <- 20:21
##   tmp[8,1] <- NA
##   tmp[3,2] <- NA
##   influx_write(con = con, db = "test", x = tmp, measurement = "test_int", use_integers = T)
##   influx_query(con = con, db = "test", query = "SELECT * from test_int")

##   # character matrix
##   tmp[,1] <- paste0("NAME", 1:10)
##   tmp[,2] <- paste0("NAME_TWO_", 1:10)
##   tmp[8,1] <- NA
##   tmp[3,2] <- NA
##   influx_write(con = con, db = "test", x = tmp, measurement = "test_str")
##   influx_query(con = con, db = "test", query = "SELECT * from test_str")

##   # mixed data.frame
##   tmp_length <- 1e2
##   tmp1 <- xts::xts(x = 1:tmp_length,
##                    order.by =  seq(as.POSIXct("1970-1-1"), by = "mins", length.out = tmp_length))
##   tmp2 <- xts::xts(x = paste0("NAME_TWO_", 1:tmp_length),
##                    order.by =  zoo::index(tmp1))
##   tmp1[8] <- NA
##   tmp2[3] <- NA
##   colnames(tmp1) <- c("one")
##   colnames(tmp2) <- c("two")
##   system.time(influx_write(con = con, db = "test", x = tmp1, measurement = "test_df"))
##   system.time(influx_write(con = con, db = "test", x = tmp2, measurement = "test_df"))
##   system.time(tmp <- influx_query(con = con, db = "test", query = "SELECT * from test_df limit 1000"))

##   # delete measurements
##   purrr::walk(paste("test", c("num", "int", "str", "df"), sep = "_"), ~ drop_measurement(con, "test", .))

## })

## testthat::test_that("write xts with sub-second accuracy", {

##   # only local tests
##   testthat::skip_on_cran()
##   testthat::skip_on_travis()

##   # how many digits to print?
##   options(digits.secs = 3)

##   tmp <- xts::xts(runif(10), order.by = runif(10) + Sys.time())
##   colnames(tmp) <- c("accuracy")
##   influx_write(con = con, db = "test", x = tmp, measurement = "subsecond_acc", precision = "ms")
##   tmp_subsecond <- influx_query(con = con, db = "test", query = "SELECT * from subsecond_acc ORDER BY time DESC LIMIT 10")

##   testthat::expect_is(object = tmp_subsecond, class = "list")

##   # delete measurement
##   drop_measurement(con, "test", "subsecond_acc")

## })

testthat::test_that("write data.frame with single measurement", {

  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()

  dfline <-
    influxdbr:::convert_to_line_protocol.data.frame(
      x = df %>% dplyr::mutate(tag_two = as.factor(tag_two)),
      measurement = "test",
      tag_cols = c("tag_one,", "tag_two", "tag three"),
      time_col = "time",
      precision = "s",
      use_integers = FALSE) %>% strsplit("\n") %>% unlist()

  testthat::expect_equal(
    dfline,
    c("test,tag_one\\,=A,tag_two=B\\,C,tag\\ three=B\\ tag field_chr=\" B,C\",field_float=0.307085896842182,field_int=21140856,field_bool=TRUE -2700", 
      "test,tag_one\\,=C,tag_two=B\\ C,tag\\ three=C\\ tag field_chr=\" B C\",field_float=0.207713897805661,field_int=59757530,field_bool=FALSE -2400", 
      "test,tag_one\\,=B,tag_two=B\\=C,tag\\ three=E\\ tag field_chr=\" B=C\",field_float=0.884227027418092,field_int=22990589,field_bool=FALSE -2100", 
      "test,tag_one\\,=E,tag_two=B,tag\\ three=D\\ tag field_chr=\" \"D\" \",field_float=0.780358511023223,field_int=12348724,field_bool=FALSE -1800", 
      "test,tag_one\\,=C,tag_two=D,tag\\ three=A\\ tag field_chr=\"  E field\",field_float=0.491231821943074,field_int=25339066,field_bool=FALSE -1500", 
      "test,tag_one\\,=C,tag_two=D,tag\\ three=C\\ tag field_chr=\"  D field\",field_float=0.603324356488883,field_int=59132106,field_bool=FALSE -1200", 
      "test,tag_one\\,=A,tag_two=B,tag\\ three=D\\ tag field_chr=\"  E field\",field_float=0.827303449623287,field_int=27488667,field_bool=TRUE -900", 
      "test,tag_one\\,=C,tag_two=B,tag\\ three=C\\ tag field_chr=\"  B field\",field_float=0.777584439376369,field_int=23569431,field_bool=TRUE -600", 
      "test,tag_one\\,=B,tag_two=E,tag\\ three=D\\ tag field_chr=\"  E field\",field_float=0.865120546659455,field_int=19867908,field_bool=TRUE -300", 
      "test,tag_one\\,=B,tag_two=D,tag\\ three=C\\ tag field_chr=\"  C field\",field_float=0.330660525709391,field_int=33052985,field_bool=FALSE 0"))

  influxdbr::drop_measurement(con, "test", "df2")

  # write full df
  influxdbr::influx_write(x = df,
                          con = con,
                          db = "test",
                          measurement = "df2",
                          time_col = "time",
                          tag_cols = c("tag_one,", "tag_two", "tag three"),
                          max_points = 2,
                          use_integers = TRUE)

  # query the data to check once again
  df2 <-
    influx_query(
      con = con,
      db = "test",
      query = "select * from df2",
      return_xts = FALSE)[[1]][, names(df)] %>%
    dplyr::mutate(field_int = as.integer(field_int))

  # cannot use expect_equal due to a range of bugs in testthat
  testthat::expect_equal(DF(df), DF(df2))

  # write df without tags
  influx_write(x = df,
    con = con,
    db = "test",
    measurement = "df3",
    time_col = "time",
    use_integers = TRUE)

  df3 <-
    influx_query(
      con = con,
      db = "test",
      query = "select * from df3",
      return_xts = FALSE)[[1]][, names(df)]

  testthat::expect_equal(DF(df), DF(df3))

  # write df without time
  dfa <- df %>%
    dplyr::mutate(time_chr = as.character(time)) %>%
    dplyr::select(-time)
  influx_write(x = dfa,
                 con = con,
                 db = "test",
                 measurement = "df4",
                 tag_cols = c("tag_one,", "tag_two", "tag three"),
                 use_integers = TRUE)

  df4 <-
    influx_query(
      con = con,
      db = "test",
      query = "select * from df4",
      return_xts = FALSE,
      timestamp_format = "ms")[[1]][, names(dfa)] %>% dplyr::arrange(time_chr)

    testthat::expect_equal(DF(dfa), DF(df4))

  # write only data
  dfb <- df %>%
    dplyr::mutate(time_chr = as.character(time)) %>%
    dplyr::select(-time)
  influx_write(x = dfb,
    con = con,
    db = "test",
    # if no time and no tags are supplied, only one point is written
    # remember: points which don't have timestamps will get the same
    # timestamp for the batch
    # therefore set at least a unique dummy tag:
    tag_cols = "time_chr",
    measurement = "df5",
    precision = "ns",
    points = 1,
    use_integers = TRUE)

  df5 <-
    influx_query(
      con = con,
      db = "test",
      query = "select * from df5",
      return_xts = FALSE,
      timestamp_format = "ms")[[1]][, names(dfb)] %>% dplyr::arrange(time_chr)

  testthat::expect_equal(DF(dfb), DF(df5))


  ## tmp_xts <-
  ##   influxdbr::influx_query(
  ##     con = con,
  ##     db = "test",
  ##     query = "select * from new_df2 group by *",
  ##     return_xts = TRUE
  ##   )

  # delete measurements
  purrr::walk(paste0("df", 2:5), ~ drop_measurement(con, "test", .))
})

testthat::test_that("write data.frame with multiple measurements", {

  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()

  library(magrittr)

  df1 <- dplyr::bind_cols(df, measurement = rep(c("one", "two", "three", "four", "five"), 2))

  # NAs not supported!
  # df[c(3, 8), c(5)] <- NA
  # df[c(4), c(6)] <- NA
  # df[c(2), c(7)] <- NA
  # df[c(4,6), c(7)] <- NA

  dfline <- influxdbr:::convert_to_line_protocol.data.frame(x = df1 %>% dplyr::mutate(tag_two = as.factor(tag_two)),
                                                            tag_cols = c("tag_one,", "tag_two", "tag three"),
                                                            time_col = "time",
                                                            measurement_col = "measurement",
                                                            precision = "s",
                                                            use_integers = FALSE)

  expect_equal(dfline,
               c("one,tag_one\\,=A,tag_two=B\\,C,tag\\ three=B\\ tag field_chr=\" B,C\",field_float=0.307085896842182,field_int=21140856,field_bool=TRUE,measurement=\"one\" -2700", 
                 "two,tag_one\\,=C,tag_two=B\\ C,tag\\ three=C\\ tag field_chr=\" B C\",field_float=0.207713897805661,field_int=59757530,field_bool=FALSE,measurement=\"two\" -2400", 
                 "three,tag_one\\,=B,tag_two=B\\=C,tag\\ three=E\\ tag field_chr=\" B=C\",field_float=0.884227027418092,field_int=22990589,field_bool=FALSE,measurement=\"three\" -2100", 
                 "four,tag_one\\,=E,tag_two=B,tag\\ three=D\\ tag field_chr=\" \"D\" \",field_float=0.780358511023223,field_int=12348724,field_bool=FALSE,measurement=\"four\" -1800", 
                 "five,tag_one\\,=C,tag_two=D,tag\\ three=A\\ tag field_chr=\"  E field\",field_float=0.491231821943074,field_int=25339066,field_bool=FALSE,measurement=\"five\" -1500", 
                 "one,tag_one\\,=C,tag_two=D,tag\\ three=C\\ tag field_chr=\"  D field\",field_float=0.603324356488883,field_int=59132106,field_bool=FALSE,measurement=\"one\" -1200", 
                 "two,tag_one\\,=A,tag_two=B,tag\\ three=D\\ tag field_chr=\"  E field\",field_float=0.827303449623287,field_int=27488667,field_bool=TRUE,measurement=\"two\" -900", 
                 "three,tag_one\\,=C,tag_two=B,tag\\ three=C\\ tag field_chr=\"  B field\",field_float=0.777584439376369,field_int=23569431,field_bool=TRUE,measurement=\"three\" -600", 
                 "four,tag_one\\,=B,tag_two=E,tag\\ three=D\\ tag field_chr=\"  E field\",field_float=0.865120546659455,field_int=19867908,field_bool=TRUE,measurement=\"four\" -300", 
                 "five,tag_one\\,=B,tag_two=D,tag\\ three=C\\ tag field_chr=\"  C field\",field_float=0.330660525709391,field_int=33052985,field_bool=FALSE,measurement=\"five\" 0"))

  # write full df
  df1a <- df1 %>% dplyr::mutate(field_int = as.integer(field_int))

  influx_write(x = df1a,
               con = con,
               db = "test",
               measurement_col = "measurement",
               time_col = "time",
               tag_cols = c("tag_one,", "tag_two", "tag three"),
               use_integers = TRUE)

  df2 <-
    influx_query(
      con = con,
      db = "test",
      query = paste("select * from",
                    c("one", "two", "three", "four", "five"),
                    "group by *", collapse = ";"),
      return_xts = FALSE) %>% dplyr::bind_rows() %>% .[, names(df1a)] %>% dplyr::arrange(time)

  expect_equal(DF(dplyr::arrange(df1a, time)), DF(df2))

  # delete measurements
  purrr::walk(c("one", "two", "three", "four", "five"), ~ drop_measurement(con, "test", .))

})

testthat::test_that("UTF-8 encodings", {

  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()

  library(magrittr)

  df <- tibble::tibble(time = seq(from = Sys.time(), by = "-5 min", length.out = 2),
                       value = runif(2),
                       unit = c("µg", "m³")) %>%
    dplyr::arrange(time)

  # write full df
  influxdbr::influx_write(x = df,
                          con = con,
                          measurement = "utf",
                          db = "test",
                          time_col = "time",
                          tag_cols = "unit")

  res <-
    influxdbr::influx_select(
      con = con,
      db = "test",
      measurement = "utf",
      field_keys = "value",
      group_by = "*",
      return_xts = FALSE
    )

  # delete measurements
  drop_measurement(con, "test", "utf")

  testthat::expect_equal(res[[1]]$unit, df$unit)

})
