# query large ones
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
