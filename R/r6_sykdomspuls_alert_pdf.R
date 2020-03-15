#' normomo
#' @import R6
#' @export sykdomspuls_alert_pdf
sykdomspuls_alert_pdf <- R6::R6Class(
  "sykdomspuls_alert_pdf",
  portable = FALSE,
  cloneable = FALSE,
  inherit = UIBase,
  list(
    run_all = function() {
      # check to see if it can run
      rundate <- fd::get_rundate()
      run <- TRUE
      if ("ui_sykdomspuls_alert_pdf" %in% rundate$package) {
        if (rundate[package == "ui_sykdomspuls_alert_pdf"]$date_extraction >= rundate[package == "sykdomspuls"]$date_extraction) run <- FALSE
      }
      if (!run & fd::config$is_production) {
        return()
      }

      if (actions[["sykdomspuls_alert_pdf"]]$can_perform_action()) {
        sykdomspuls_std_alerts_pdf()
        actions[["sykdomspuls_alert_pdf"]]$action_performed()
      }

      # update rundate
      fd::update_rundate(
        package = "ui_sykdomspuls_alert_pdf",
        date_extraction = rundate[package == "sykdomspuls"]$date_extraction,
        date_results = rundate[package == "sykdomspuls"]$date_results,
        date_run = lubridate::today()
      )
    }
  )
)

sykdomspuls_std_alerts_pdf <- function() {
  fd::msg("Sykdomspuls: alerts pdf start", slack = T)

  max_yrwk <- fhi::isoyearweek(fd::get_rundate()[package == "sykdomspuls"]$date_results - 6)
  tag_relevant <- sykdomspuls::CONFIG$MODELS$standard[alertExternal == T]$tag

  d <- fd::tbl("spuls_standard_results") %>%
    dplyr::filter(granularity_time == "weekly") %>%
    dplyr::filter(granularity_geo == "municip") %>%
    dplyr::filter(yrwk == !!max_yrwk) %>%
    dplyr::filter(status == "High") %>%
    dplyr::filter(tag %in% !!tag_relevant) %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()

  if (nrow(d) == 0) {
    return()
  }

  d <- unique(d[, c("tag", "location_code", "location_name")])
  d[sykdomspuls::CONFIG$MODELS$standard, on = "tag", name_short := namesShort]
  d[sykdomspuls::CONFIG$MODELS$standard, on = "tag", name_long := namesLong]
  d[, output_file := glue::glue(
    "{name_short}_{location_name}.pdf",
    name_short = name_short,
    location_name = location_name
  )]

  fs::dir_create(fd::path("results", sykdomspuls_date(), "standard", "alert_pdfs", package = "sykdomspuls"))
  d[, output_dir := fd::path("results", sykdomspuls_date(), "standard", "alert_pdfs", package = "sykdomspuls")]
  d[, attachment := fs::path(output_dir, output_file)]

  fd::msg(glue::glue("Sykdomspuls: creating n={nrow(d)} alerts pdf"), slack = T)

  for (i in 1:nrow(d)) {
    Sys.sleep(1)

    input <- system.file("extdata", "alert.Rmd", package = "ui")

    output_file <- d$output_file[i]
    output_dir <- d$output_dir[i]
    x_location_code <- d$location_code[i]
    x_tag <- d$tag[i]
    x_name_long <- d$name_long[i]

    fhi::RenderExternally(
      input = input,
      output_file = output_file,
      output_dir = output_dir,
      params = as.character(glue::glue(
        "location_code=\"{x_location_code}\",",
        "tag=\"{x_tag}\",",
        "name_long=\"{x_name_long}\",",
        "max_yrwk=\"{max_yrwk}\""
      ))
    )
  }

  tab <- huxtable::hux(
    Syndrom = d$name_long,
    Kommunenavn = d$location_name,
    Kommunenummer = d$location_code,
    file = d$output_file
  ) %>%
    huxtable::add_colnames() %>%
    huxtable::theme_basic() %>%
    huxtable::to_html()

  html <- glue::glue(
    "Please find attached sykdomspuls alert pdfs.<br>",
    "These are the municipalities with at least one z-score above 4<br><br>",
    "{tab}"
  )

  attachments <- d$attachment
  if (length(attachments) > 10) attachments <- attachments[1:10]


  fd::mailgun(
    subject = "Sykdomspuls alert pdfs",
    html = html,
    bcc = fd::e_emails(
      "sykdomspuls_utbrudd",
      is_final = actions[["sykdomspuls_alert_pdf"]]$is_final()
    ),
    attachments = attachments,
    is_final = actions[["sykdomspuls_alert_pdf"]]$is_final()
  )

  fd::msg("Sykdomspuls: alerts pdf finished", slack = T)
}
