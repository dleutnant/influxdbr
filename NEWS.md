# influxdbr 0.13.0.9000 - Do not use in production!

* testing `influx_write_df` to upload mulitple tags and fields per timestamp (#20).

Warning: final test still to be done. NA's are currently not supported.
Also breaking change to be expected in `influx_write()` api!

# influxdbr 0.13.0

* Fix coercion error in `influx_write()` in case of sub-second accuracy (#25).

* `influx_select()` correctly parses integer arguments (#27).

* Significantly improved performance of `influx_query()` (#28).

# influxdbr 0.12.0

* First CRAN release