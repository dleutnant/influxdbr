context("influx_write")

test_that("setup", {
  skip_on_cran()
  skip_on_travis()
  expect_silent(setup_database())
  
})

DF <- as.data.frame

## setup df
time0 <- .POSIXct(0, tz = "UTC")
set.seed(100)
df <- data.frame(time = seq(from = time0, by = "-5 min", length.out = 10),
                 `tag_one,` = sample(LETTERS[1:5], 10, replace = T),
                 tag_two = sample(LETTERS[1:5], 10, replace = T),
                 `tag three` = sample(paste(LETTERS[1:5], "tag"), 10, replace = T),
                 field_chr = sample(paste(" ", LETTERS[1:5], "field"), 10, replace = T),
                 field_float = stats::runif(10),
                 field_int = sample(1:100000000,10),
                 field_bool = stats::runif(10) > 0.5,
                 stringsAsFactors = F,
                 check.names = F) %>% dplyr::arrange(time)
df[1:3, "tag_two"] <- c("B,C", "B C", "B=C")
df[1:4, "field_chr"] <- c(" B,C", " B C", " B=C", " \"D\" ")

# NAs not supported!
# df[c(3, 8), c(5)] <- NA
# df[c(4), c(6)] <- NA
# df[c(2), c(7)] <- NA
# df[c(4,6), c(7)] <- NA


test_that("write data.frame with single measurement", {
  skip_on_cran()
  skip_on_travis()

  dfline <-
    influxdbr:::convert_to_line_protocol.data.frame(
      x = df %>% dplyr::mutate(tag_two = as.factor(tag_two)),
      measurement = "test",
      tag_cols = c("tag_one,", "tag_two", "tag three"),
      time_col = "time",
      precision = "s",
      use_integers = FALSE) %>% strsplit("\n") %>% unlist()

  expect_equal(
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

  drop_measurement(CON, DB, "df2")

  # write full df
  influx_write(x = df, CON, DB,
               measurement = "df2",
               time_col = "time",
               tag_cols = c("tag_one,", "tag_two", "tag three"),
               max_points = 2,
               use_integers = TRUE)

  # query the data to check once again
  df2 <-
    influx_query(CON, DB,
                 query = "select * from df2")[, names(df)] %>%
    dplyr::mutate(field_int = as.integer(field_int))

  # cannot use expect_equal due to a range of bugs in testthat
  expect_equal(DF(df), DF(df2))

  # write df without tags
  influx_write(x = df, CON, DB,
               measurement = "df3",
               time_col = "time",
               use_integers = TRUE)

  df3 <-
    influx_query(CON, DB,
      query = "select * from df3")[, names(df)]

  expect_equal(DF(df), DF(df3))

  # write df without time
  dfa <- df %>%
    dplyr::mutate(time_chr = as.character(time)) %>%
    dplyr::select(-time)
  influx_write(x = dfa, CON, DB,
               measurement = "df4",
               tag_cols = c("tag_one,", "tag_two", "tag three"),
               use_integers = TRUE)

  df4 <-
    influx_query(CON, DB,
                 query = "select * from df4",
                 timestamp_format = "ms")[, names(dfa)] %>%
    dplyr::arrange(time_chr)

  expect_equal(DF(dfa), DF(df4))

  # write only data
  dfb <- df %>%
    dplyr::mutate(time_chr = as.character(time)) %>%
    dplyr::select(-time)
  influx_write(x = dfb, CON, DB,
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
    influx_query(CON, DB,
      query = "select * from df5",
      timestamp_format = "ms")[, names(dfb)] %>% dplyr::arrange(time_chr)

  expect_equal(DF(dfb), DF(df5))


  # delete measurements
  for (nm in paste0("df", 2:5))
    drop_measurement(CON, DB, nm)
  
})

test_that("write data.frame with multiple measurements", {
  skip_on_cran()
  skip_on_travis()

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

  influx_write(x = df1a, CON, DB,
               measurement_col = "measurement",
               time_col = "time",
               tag_cols = c("tag_one,", "tag_two", "tag three"),
               use_integers = TRUE)

  df2 <-
    influx_query(CON, DB,
                 query = paste("select * from",
                               c("one", "two", "three", "four", "five"),
                               "group by *", collapse = ";"),
                 tags_as_factors = FALSE) %>%
    dplyr::bind_rows() %>% .[, names(df1a)] %>% dplyr::arrange(time)

  expect_equal(DF(dplyr::arrange(df1a, time)), DF(df2))

  # delete measurements
  for (nm in c("one", "two", "three", "four", "five"))
      drop_measurement(CON, DB, nm)

})

test_that("UTF-8 encodings", {
  skip_on_cran()
  skip_on_travis()

  df <- data.frame(time = seq(from = Sys.time(), by = "-5 min", length.out = 2),
                   value = runif(2),
                   unit = c("µg", "m³"),
                   stringsAsFactors = T) %>%
    dplyr::arrange(time)

  # write full df
  influx_write(x = df, CON, DB,
               measurement = "utf", 
               time_col = "time",
               tag_cols = "unit")

  res <-
    influx_select(CON, DB,
      measurement = "utf",
      field_keys = "value",
      group_by = "*")

  # delete measurements
  drop_measurement(CON, DB, "utf")

  expect_equal(res$unit, df$unit)

})

test_that("write data.table with multiple measurements", {
  skip_on_cran()
  skip_on_travis()
  
  dt <- data.table::data.table(time = seq(from = Sys.time(), by = "-5 min", length.out = 10),
                               `tag_one,` = sample(LETTERS[1:5], 10, replace = T),
                               tag_two = sample(LETTERS[1:5], 10, replace = T),
                               `tag three` = sample(paste(LETTERS[1:5], "tag"), 10, replace = T),
                               field_chr = sample(paste(" ", LETTERS[1:5], "field"), 10, replace = T),
                               field_float = stats::runif(10),
                               field_int = sample(1:100000000,10),
                               field_bool = stats::runif(10) > 0.5, 
                               measurement = rep(c("one", "two", "three", "four", "five"), 2)) %>% 
    data.table::setorder("time")
  
  influx_write(x = dt, CON, DB,
               measurement_col = "measurement",
               time_col = "time",
               tag_cols = c("tag_one,", "tag_two", "tag three"),
               use_integers = TRUE)

  meas <- c("one", "two", "three", "four", "five")
  res <-
    influx_query(CON, DB,
                 query = paste("select * from", meas, "group by *", collapse = ";"))


  for (i in seq_along(meas))
    expect_true(all(res[[i]]$measurement == meas[[i]]))
  
  # delete measurements
  for(nm in c("one", "two", "three", "four", "five"))
    drop_measurement(CON, DB, nm)
  
})
