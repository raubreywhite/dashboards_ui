#' amort
#' @import R6
#' @export amort
amort <- R6::R6Class(
  "amort",
  portable = FALSE,
  cloneable = FALSE,
  list(
    run_all = function() {
      # check to see if it can run
      rundate <- fd::get_rundate()
      run <- TRUE
      if ("ui_amort" %in% rundate$package) {
        if (rundate[package == "ui_amort"]$date_extraction >= rundate[package == "normomo"]$date_extraction) run <- FALSE
        if (rundate[package == "ui_amort"]$date_extraction >= rundate[package == "sykdomspuls"]$date_extraction) run <- FALSE
      }
      if (!run & fd::config$is_production) {
        return()
      }

      # send email
      if (actions[["ui_amort"]]$can_perform_action()) {
        amort_email_results()
        normomo_email_ssi()
        actions[["ui_amort"]]$action_performed()
      }

      # update rundate
      fd::update_rundate(
        package = "ui_amort",
        date_extraction = rundate[package == "normomo"]$date_extraction,
        date_results = rundate[package == "normomo"]$date_results,
        date_run = lubridate::today()
      )
    }
  )
)

amort_table <- function(){
  weather <- fd::get_weather()
  weather <- weather[,.(
    tx_mean=mean(tx),
    tn_mean=mean(tn)
  ),keyby=.(
    location_code,
    yrwk
  )]

  mem <- fd::tbl("spuls_mem_results") %>%
    dplyr::filter(tag == "influensa") %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()

  d <- fd::tbl("normomo_standard_results") %>%
    dplyr::filter(location_code == "norge") %>%
    dplyr::filter(age == "Total") %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()
  setorder(d, -wk)
  d <- d[1:10]

  d[weather, on=c("yrwk","location_code"), tx_mean:=tx_mean]
  d[weather, on=c("yrwk","location_code"), tn_mean:=tn_mean]
  d[mem, on =c("yrwk","location_code"), ils_rate:=rate]
  d[mem, on =c("yrwk","location_code"), ils_status:=i.status]

  tab <- huxtable::hux(
    "\u00C5r-uke" = d$yrwk,
    "Overd\u00F8dlighet\\textsuperscript{1}" = round(d$excessp),
    "Min" = fhiplot::format_nor(d$tn_mean,1),
    "Maks" = fhiplot::format_nor(d$tx_mean,1),
    "\\%" = fhiplot::format_nor(d$ils_rate, 2),
    "Status" = d$ils_status
  ) %>%
    huxtable::add_colnames() %>%
    fhiplot::huxtable_theme_fhi_basic() %>%
    huxtable::set_align(huxtable::everywhere, huxtable::everywhere, "center") %>%
    huxtable::set_top_padding(huxtable::everywhere, huxtable::everywhere, 0.1) %>%
    huxtable::set_bottom_padding(huxtable::everywhere, huxtable::everywhere, 0.1) %>%
    huxtable::set_left_padding(huxtable::everywhere, huxtable::everywhere, 0.1) %>%
    huxtable::set_right_padding(huxtable::everywhere, huxtable::everywhere, 0.1)

  index_low <- which(d$status == "normal") + 1
  index_med <- which(d$status == "medium") + 1
  index_hig <- which(d$status == "high") + 1

  for(col in 2:2){
    huxtable::background_color(tab)[-1, col] <- fhiplot::warning_color["low"]
    huxtable::background_color(tab)[index_med, col] <- fhiplot::warning_color["med"]
    huxtable::background_color(tab)[index_hig, col] <- fhiplot::warning_color["hig"]
  }

  tab <- huxtable::add_rows(tab, tab[1, ], after = 0)

  tab <- huxtable::merge_cells(tab, 1, 1:2)
  tab[1, 1] <- " "

  tab <- huxtable::merge_cells(tab, 1, 3:4)
  tab[1, 3] <- "Temperatur\\textsuperscript{2}"

  tab <- huxtable::merge_cells(tab, 1, 5:6)
  tab[1, 5] <- "ILS\\textsuperscript{3}"

  nr0 <- nrow(tab)+1

  tab <- huxtable::add_footnote(tab, glue::glue(
    "\\textsuperscript{1}Differansen mellom antall forventede og antall korrigerte d{fhi::nb$oe}dsfall\\\\*",
    "\\textsuperscript{2}Gjennomsnit av dagligeverdier, vektet etter befolkningstall\\\\*",
    "\\textsuperscript{3}Influenza-like symptoms"
  ), border = 0)

  nr1 <- nrow(tab)

  huxtable::escape_contents(tab)[nr0:nr1, ] <- F
  huxtable::escape_contents(tab)[1:2, ] <- F

  huxtable::left_border_style(tab)[,3] <- "double"
  huxtable::left_border_style(tab)[,5] <- "double"

  huxtable::width(tab) <- 0.8

  #tab
  tab1_name <- "table1.png"
  tab1 <- fs::path(fhi::temp_dir(), tab1_name)
  #tab1 <- fs::path("/git", tab1_name)
  fd::huxtable_to_png(tab, file = tab1)

  html <- glue::glue(
    "<html>",
    "<h2>Dette er en TEST av data sammensl{fhi::nb$aa}ing. Ikke ta resultatene for seri{fhi::nb$oe}s</h2>",
    "<b>Tabell 1.</b> .<br><br>",
    "<img src='cid:{tab1_name}' width='800' align='middle' style='display:block;width:100%;max-width:800px' alt=''><br><br>",
    "</html>"
  )

  fd::mailgun(
    subject = glue::glue("Tilskrivbar d{fhi::nb$oe}delighet {normomo_yrwk()}"),
    html = html,
    to = "dashboardsfhi@gmail.com",
    bcc = fd::e_emails("normomo_results", is_final = actions[["amort"]]$is_final()),
    inlines = c(tab1),
    is_final = actions[["amort"]]$is_final()
  )

}
