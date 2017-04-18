## 0.11.9

### Release Notes

- Improved parsing performance (json to tibble)
- No new features added

## 0.11.8

### Release Notes

- Package now applies tidyverse pradigm.
- Improved documentation

### Features

- handling of chunked data 

## 0.11.4

### Release Notes

- This release will be used to submit package to CRAN (planned for 03/2017).
- Edit 17/04/13: CRAN submission rescheduled due to major changes in package structure.

### Features

- [#9](https://github.com/dleutnant/influxdbr/pull/9) Add support for https and prefix path (thanks to @gedejong)
- [#2](https://github.com/dleutnant/influxdbr/pull/2) Add time precision handling for write requests (thanks to @mvadu)
- [#5](https://github.com/dleutnant/influxdbr/pull/5) influx_write now allows non-numerical xts (thanks to @williampeterpaul)
- introducing chunked parameter in influx_query 

### Bugfixes

- show_stats returns diagnostics correctly
- show_series takes care of measurements with no attributes


