
parse_csv_data.table <- function(x) {
  data.table::fread(text = x)
}

parse_csv <- local({
  rc <- NULL
  function(lst) {
    if (is.null(rc)) {
      rc <<-
        switch(getOption("influxdbr.parse_csv.backend", "auto"),
               data.table = parse_csv_data.table,
               readr = readr::read_csv,
               utils = , 
               base = utils::read.csv,
               if (requireNamespace("data.table", quietly = TRUE)) {
                 parse_csv_data.table
               } else if (requireNamespace("readr", quietly = TRUE)) {
                 readr::read_csv
               } else {
                 utils::read.csv
               })
    }
    rc(lst)
  }
})


## environment(read_csv)[["rc"]] <- parse_csv_data.table
## environment(read_csv)[["rc"]] <- readr::read_csv
## environment(read_csv)[["rc"]] <- utils::read.csv

