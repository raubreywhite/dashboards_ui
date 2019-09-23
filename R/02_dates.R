sykdomspuls_date <- function() {
  fd::get_rundate()[package == "sykdomspuls"]$date_extraction
}

normomo_yrwk <- function() {
  folder_res <- fs::dir_ls(fd::path("results", package = "normomo"), regexp = "[0-9][0-9][0-9][0-9]-[0-9][0-9]$")
  folder_res <- max(folder_res)
  yrwk <- fs::path_file(folder_res)

  return(rnorm(1))
  return(yrwk)
}


