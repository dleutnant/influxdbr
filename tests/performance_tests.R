# query large ones
system.time(tmp <- influx_select(con, "stbmod", "value", measurement = "Durchfluss", where = "Version = '5'", return_xts = T, limit = 10))

system.time(tmp1 <- influx_select(con, "stbmod", "value", measurement = "Durchfluss", where = "Version = '5'", return_xts = T))

system.time(tmp2 <- influx_select(con, "stbmod", "value", measurement = "Durchfluss", return_xts = T))

system.time(tmp3 <- influx_select(con, "stbmod", "value", measurement = "Durchfluss, Temperatur", group_by = "Ort", return_xts = T, limit = 100))


> system.time(tmp1 <- influx_select(con, "stbmod", "value", measurement = "Durchfluss", where = "Version = '5'", return_xts = T))
[1] "2016-03-09 17:41:19 before query"
[1] "2016-03-09 17:41:38 before json"
[1] "2016-03-09 17:42:33 before list_of_series"
[1] "2016-03-09 17:42:33 before value extraction"
[1] "2016-03-09 17:42:39 before type.convert"
[1] "2016-03-09 17:43:10 before type.convert2"
[1] "2016-03-09 17:43:11 before time convert"
[1] "2016-03-09 17:43:12 after list_of_series"
user  system elapsed
68.84   26.03  113.10


[1] "2016-03-09 19:03:20 before query"
[1] "2016-03-09 19:03:40 before json"
[1] "2016-03-09 19:04:34 before list_of_series"
[1] "2016-03-09 19:04:34 before value extraction"
[1] "2016-03-09 19:04:34 before type.convert"
[1] "2016-03-09 19:04:47 after type.convert"
[1] "2016-03-09 19:04:50 after list_of_series"


system.time(test1 <- Reduce(cbind, all_values[1:2]))
str(test1)

system.time(test1 <- as.data.frame(do.call(cbind, all_values[1:2])))


system.time(tmp <- sapply(all_values, rbind))
system.time(tmp <- lapply(all_values, rbind))

system.time(values_is <- as.data.frame(do.call(rbind, all_values),
                                       stringsAsFactors = FALSE,
                                       row.names = NULL))

system.time(values_old <- as.data.frame(do.call(rbind, lapply(all_values, unlist)),
                                        stringsAsFactors = FALSE,
                                        row.names = NULL))

system.time(values_new <- as.data.frame(matrix(unlist(all_values),
                                               nrow = length(unlist(all_values[1]))),
                                        stringsAsFactors = FALSE,
                                        row.names = NULL))

system.time(wow <- as.data.frame(t(matrix(unlist(all_values),
                                          nrow = length(unlist(all_values[1])))),
                                 stringsAsFactors = FALSE,
                                 row.names = NULL))


wow1 <- wow2 <- wow
system.time(mode(wow1) <- "character")
system.time(wow2[] <- lapply(wow2, as.character))
