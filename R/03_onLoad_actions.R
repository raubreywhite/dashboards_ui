.onLoad <- function(libname, pkgname) {
  fd::initialize(
    package = "ui",
    load_package = FALSE
  )

  tryCatch({
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
  },
  error=function(e){
    fd::msg("No database available")
  })
}
