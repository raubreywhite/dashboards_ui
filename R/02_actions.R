sykdomspuls_date <- function() {
  fd::get_rundate()[package == "sykdomspuls"]$date_extraction
}

normomo_yrwk <- function() {
  folder_res <- fs::dir_ls(fd::path("results", package = "normomo"), regexp = "[0-9][0-9][0-9][0-9]-[0-9][0-9]$")
  folder_res <- max(folder_res)
  yrwk <- fs::path_file(folder_res)

  return(yrwk)
}

actions <- new.env()
actions[["sykdomspuls_obs"]] <- fd::action$new(
  key = "ui_sykdomspuls_obs",
  value = fhi::isoyearweek(sykdomspuls_date()),
  dev_always_performs = TRUE,
  production_days = c(3:5),
  first_date_of_production = "2019-09-21"
)

actions[["sykdomspuls_alert_pdf"]] <- fd::action$new(
  key = "sykdomspuls_alert_pdf",
  value = fhi::isoyearweek(sykdomspuls_date()),
  dev_always_performs = TRUE,
  production_days = c(3:5),
  first_date_of_production = "2019-09-21"
)

actions[["normomo_email"]] <- fd::action$new(
  key = "normomo_email",
  value = normomo_yrwk(),
  dev_always_performs = TRUE,
  production_days = c(2:3),
  first_date_of_production = "2019-09-21"
)
