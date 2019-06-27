library(testthat)
library(influxdbr)
library(magrittr)
library(dplyr)

unfactor <- function(df) {
  for (nm in names(df))
    if (is.factor(df[[nm]]))
      df[[nm]] <- as.character(df[[nm]])
  df
}

DB <- "INFLUXDBRTEST"

A <- data.frame(time = .POSIXct(1:3, tz = "UTC"),
                a = 1:3, b = factor(letters[1:3]), c = 1, d = letters[4:6], 
                stringsAsFactors = FALSE)
class(A) <- c("influxdbr.response", "data.frame")
As <- unfactor(A)

B <- data.frame(time = .POSIXct(2:4, tz = "UTC"),
                A = 1:3, B = factor(LETTERS[1:3]), c = 2,
                stringsAsFactors = FALSE)
class(B) <- c("influxdbr.response", "data.frame")
Bs <- unfactor(B)

C <- data.frame(time = .POSIXct(2:4, tz = "UTC"),
                K = 1:3, L = factor(LETTERS[1:3]), M = 2,
                stringsAsFactors = FALSE)
class(C) <- c("influxdbr.response", "data.frame")
Cs <- unfactor(C)

setup_database <- function() {
  CON <<- influx_connection(group = "admin")
  drop_database(CON, DB)
  create_database(CON, DB)
  influx_write(A, CON, DB, measurement = "A", time_col = "time", tag_cols = "b")
  influx_write(B, CON, DB, measurement = "B", time_col = "time", tag_cols = "B")
}

setup_database()

test_check("influxdbr")

## influx_write(A, CON, "test2", measurement = "A", time_col = "time", tag_cols = "b")
## influx_write(C, CON, "test2", measurement = "C", time_col = "time", tag_cols = "L")
