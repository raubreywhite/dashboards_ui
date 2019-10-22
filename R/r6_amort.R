#' amort
#' @import R6
#' @export amort
amort <- R6::R6Class(
  "amort",
  portable = FALSE,
  cloneable = FALSE,
  inherit = UIBase,
  list(
    run_all = function() {
      # check to see if it can run
      if (!fd::exists_rundate("brain_amort")) {
        return()
      }
      rundate <- fd::get_rundate()

      run <- TRUE
      if (fd::exists_rundate("brain_amort") & fd::exists_rundate("ui_amort")) {
        if (rundate[package == "ui_amort"]$date_extraction >= rundate[package == "brain_amort"]$date_extraction) {
          run <- FALSE
        }
      }

      if (!run & fd::config$is_production) {
        return()
      }

      fd::msg("This is the amort_folder")
      fd::msg(amort_folder())

      fs::dir_create(amort_folder())
      amort_table_1()
      amort_rr_graphs()

      # send email
      if (actions[["ui_amort"]]$can_perform_action()) {
        amort_email_results()
        actions[["ui_amort"]]$action_performed()
      }

      date_extraction <- max(
        rundate[package == "normomo"]$date_extraction,
        rundate[package == "sykdomspuls"]$date_extraction
      )

      date_results <- max(
        rundate[package == "normomo"]$date_results,
        rundate[package == "sykdomspuls"]$date_results
      )

      # update rundate
      fd::update_rundate(
        package = "ui_amort",
        date_extraction = date_extraction,
        date_results = date_results,
        date_run = lubridate::today()
      )
    }
  )
)

amort_folder <- function() {
  fd::results_folder("amort", fd::get_rundate()[package == "brain_amort"]$date_extraction)
}

amort_season_graphs <- function() {
  d <- fd::tbl("brain_amort_results") %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()
  d[exposure %in% c(
    "A_H1N1_per1000",
    "A_H3N2_per1000",
    "B_per1000"
    ),exposure:="ils_with_virology"]
  d <- d[,.(
    attr_est=sum(attr_est),
    attr_low=sum(attr_low),
    attr_high=sum(attr_high)
  ),keyby=.(
    location_code,
    season,
    yrwk,
    date,
    exposure,
    exposure_value
  )]

  dates <- unique(d[,c("season", "date")])
  setorder(dates, date)
  d[,season:=factor(season, levels = dates$season)]

  q <- ggplot(d, aes(x=season, y=attr_est, ymin=attr_low, ymax=attr_high, color = exposure_value))
  q <- q + geom_pointrange()
  q <- q + fhiplot::theme_fhi_lines()
  q <- q + fhiplot::set_x_axis_vertical()
  q

  mem <- fd::tbl("spuls_mem_results") %>%
    dplyr::filter(tag == "influensa") %>%
    dplyr::filter(location_code == "norge") %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()
  mem[,status:=factor(
    status,
    levels=c(
      "veryhigh",
      "high",
      "medium",
      "low",
      "verylow"
    ),
    ordered=T
    )]
  mem <- mem[,.(
    max_ils = max(rate),
    status = min(status)
  ), keyby=.(
    location_code,
    season
  )]
  d[mem,on=c(
    "location_code",
    "season"
  ),max_ils:=max_ils]
  d[mem,on=c(
    "location_code",
    "season"
  ),mem_status:=status]


  fake_data <- d[exposure=="ili_per10000"]
  fake_data[c(1:5),mem_status:=c(
    "veryhigh",
    "high",
    "medium",
    "low",
    "verylow"
  )]

  q <- ggplot(d[exposure=="ili_per10000"], aes(
    x=max_ils,
    y=attr_est,
    ymin=attr_low,
    ymax=attr_high))
  q <- q + geom_pointrange(size=1.5)
  q <- q + geom_point(data=fake_data,mapping=aes(color=mem_status),alpha=0)
  q <- q + geom_point(mapping=aes(color=mem_status),size=5)
  q <- q + ggrepel::geom_label_repel(mapping=aes(label=season))
  q <- q + fhiplot::scale_color_fhi(palette="map_div_complete", direction = -1)
  q <- q + fhiplot::theme_fhi_lines()
  q <- q + scale_x_continuous("Max weekly percentage of consultations that are ILS")
  q <- q + scale_y_continuous("Estimated attributable mortality")
  fhiplot::save_a4(q, fs::path(amort_folder(), "fig3.png"), landscape = T)
}

amort_table_1 <- function() {
  weather <- fd::get_weather()
  weather <- weather[, .(
    tx_mean = mean(tx),
    tn_mean = mean(tn)
  ), keyby = .(
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

  d[weather, on = c("yrwk", "location_code"), tx_mean := tx_mean]
  d[weather, on = c("yrwk", "location_code"), tn_mean := tn_mean]
  d[mem, on = c("yrwk", "location_code"), ils_rate := rate]
  d[mem, on = c("yrwk", "location_code"), ils_status := i.status]

  tab <- huxtable::hux(
    "\u00C5r-uke" = d$yrwk,
    "Overd\u00F8delighet\\textsuperscript{1}" = round(d$excessp),
    "Min" = fhiplot::format_nor(d$tn_mean, 1),
    "Maks" = fhiplot::format_nor(d$tx_mean, 1),
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

  for (col in 2:2) {
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

  nr0 <- nrow(tab) + 1

  tab <- huxtable::add_footnote(tab, glue::glue(
    "\\textsuperscript{1}Differansen mellom antall forventede og antall korrigerte d{fhi::nb$oe}dsfall\\\\*",
    "\\textsuperscript{2}Gjennomsnit av dagligeverdier, vektet etter befolkningstall\\\\*",
    "\\textsuperscript{3}Influenza-like symptoms"
  ), border = 0)

  nr1 <- nrow(tab)

  huxtable::escape_contents(tab)[nr0:nr1, ] <- F
  huxtable::escape_contents(tab)[1:2, ] <- F

  huxtable::left_border_style(tab)[, 3] <- "double"
  huxtable::left_border_style(tab)[, 5] <- "double"

  huxtable::width(tab) <- 0.8

  # tab
  tab1_name <- "table1.png"
  tab1 <- fs::path(amort_folder(), tab1_name)
  print(tab1)
  # tab1 <- fs::path("/git", tab1_name)
  fd::huxtable_to_png(tab, file = tab1)
}

amort_rr_graphs <- function() {
  x_yr <- fhi::isoyear_n(fd::get_rundate()[package == "brain_amort"]$date_results)
  rrs <- fd::tbl("brain_amort_rr") %>%
    dplyr::filter(year_train_max == !!x_yr) %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()

  rrs[fhidata::norway_locations_long_current, on = "location_code", location_name := location_name]
  min_vals <- rrs[rr_est == 1]

  lvls <- c("Norge", unique(fhidata::norway_locations_current$county_name))
  rrs[, location_name := factor(location_name, levels = lvls)]

  q <- ggplot(rrs[exposure == "tx"], aes(x = exposure_value, y = rr_est, ymin = rr_l95, ymax = rr_u95))
  q <- q + geom_ribbon(alpha = 0.5)
  q <- q + geom_line()
  q <- q + geom_hline(yintercept = 1, color = "red")
  q <- q + geom_vline(data = min_vals[exposure == "tx"], mapping = aes(xintercept = exposure_value), color = "red")
  q <- q + lemon::facet_rep_wrap(~location_name, repeat.tick.labels = "all", ncol = 4, scales = "fixed")
  q <- q + fhiplot::theme_fhi_lines()
  q <- q + fhiplot::scale_color_fhi("", palette = "contrast", direction = 1)
  q <- q + fhiplot::scale_fill_fhi("", palette = "contrast", direction = 1)
  q <- q + scale_y_continuous("Risk ratio", expand = expand_scale(mult = c(0, 0.05)))
  q <- q + scale_x_continuous("Degrees celcius")
  q <- q + labs(title = "Attributable risk of death due to max daily temperature")
  fhiplot::save_a4(q, fs::path(amort_folder(), "fig1.png"), landscape = F)

  q <- ggplot(rrs[exposure == "ilsper1000"], aes(x = exposure_value, y = rr_est, ymin = rr_l95, ymax = rr_u95))
  q <- q + geom_ribbon(alpha = 0.5)
  q <- q + geom_line()
  q <- q + geom_hline(yintercept = 1, color = "red")
  q <- q + lemon::facet_rep_wrap(~location_name, repeat.tick.labels = "all", ncol = 4, scales = "fixed")
  q <- q + fhiplot::theme_fhi_lines()
  q <- q + fhiplot::scale_color_fhi("", palette = "contrast", direction = 1)
  q <- q + fhiplot::scale_fill_fhi("", palette = "contrast", direction = 1)
  q <- q + scale_y_continuous("Risk ratio", expand = expand_scale(mult = c(0, 0.05)))
  q <- q + scale_x_continuous("Number of consultations per 1000 that are ILS")
  q <- q + labs(title = "Attributable risk of death due to number of consultations per 1000 that are ILS")
  fhiplot::save_a4(q, fs::path(amort_folder(), "fig2.png"), landscape = F)
}

amort_email_results <- function() {
  tab1_name <- "table1.png"
  tab1 <- fs::path(amort_folder(), tab1_name)

  fig1_name <- "fig1.png"
  fig1 <- fs::path(amort_folder(), fig1_name)

  fig2_name <- "fig2.png"
  fig2 <- fs::path(amort_folder(), fig2_name)

  html <- glue::glue(
    "<html>",
    "<h2>Dette er en TEST av data sammensl{fhi::nb$aa}ing. Ikke ta resultatene for seri{fhi::nb$oe}s</h2>",
    "<b>Tabell 1.</b> XXXXXXX.<br><br>",
    "<img src='cid:{tab1_name}' width='800' align='middle' style='display:block;width:100%;max-width:800px' alt=''><br><br>",
    "<b>Figur 1.</b> XXXXXXX.<br><br>",
    "<img src='cid:{fig1_name}' width='800' align='middle' style='display:block;width:100%;max-width:800px' alt=''><br><br>",
    "<b>Figur 2.</b> XXXXXXX.<br><br>",
    "<img src='cid:{fig2_name}' width='800' align='middle' style='display:block;width:100%;max-width:800px' alt=''><br><br>",
    "</html>"
  )

  fd::mailgun(
    subject = glue::glue("Tilskrivbar d{fhi::nb$oe}delighet {normomo_yrwk()}"),
    html = html,
    to = "dashboardsfhi@gmail.com",
    bcc = fd::e_emails("ui_amort", is_final = actions[["ui_amort"]]$is_final()),
    inlines = c(tab1, fig1, fig2),
    is_final = actions[["ui_amort"]]$is_final()
  )
}
