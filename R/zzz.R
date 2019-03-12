# inspired by http://r-pkgs.had.co.nz/r.html
.onLoad <- function(libname, pkgname) {
  # load options available
  op <- options()
  
  # set option
  op.influxdbr <- list(
    influxdbr.progress_bar = TRUE
  )
  
  # get the difference to available options
  toset <- !(names(op.influxdbr) %in% names(op))
  
  # save options
  if (any(toset)) options(op.influxdbr[toset])
  
  invisible()
}