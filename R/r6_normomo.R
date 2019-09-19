#' normomo
#' @import R6
#' @export normomo
normomo <- R6::R6Class(
  "normomo",
  portable = FALSE,
  cloneable = FALSE,
  list(
    run_all = function() {
      rundate <- fd::get_rundate()

      run <- TRUE
      if ("brain_normomo" %in% rundate$package) {
        if (rundate[package == "brain_normomo"]$date_extraction >= rundate[package == "normomo"]$date_extraction) run <- FALSE
      }
      if (!run & fd::config$is_production) {
        return()
      }

      # write results as excel file
      fs::dir_create(fd::path("results", normomo_yrwk(), "data", package = "normomo"))
      d <- fd::tbl("normomo_standard_results") %>%
        dplyr::collect() %>%
        fd::latin1_to_utf8()
      writexl::write_xlsx(
        d,
        path = fd::path("results", normomo_yrwk(), "data", "results.xlsx", package = "normomo")
      )

      # make graphs
      normomo_graphs()

      # send email
      normomo_email_internal()
    }
  )
)

normomo_yrwk <- function() {
  folder_res <- fs::dir_ls(fd::path("results", package = "normomo"), regexp = "[0-9][0-9][0-9][0-9]-[0-9][0-9]$")
  folder_res <- max(folder_res)
  yrwk <- fs::path_file(folder_res)

  return(yrwk)
}

normomo_email_internal <- function() {
  d <- fd::tbl("normomo_standard_results") %>%
    dplyr::filter(location_code == "norge") %>%
    dplyr::filter(age == "Total") %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()
  setorder(d, -wk)
  d <- d[1:10]

  tab <- huxtable::hux(
    "\u00C5r-uke" = d$yrwk,
    "Intet" = glue::glue("{round(d$thresholdp_0)} - {round(d$thresholdp_1)}"),
    "Lav" = glue::glue("{round(d$thresholdp_1)} - {round(d$thresholdp_2)}"),
    "H\u00F8y" = glue::glue(">{round(d$thresholdp_2)}"),
    "Reg." = d$nb,
    "Kor." = round(d$nbc),
    "Over." = round(d$excessp),
    "Z-score" = fhiplot::format_nor(d$zscore, 2)
  ) %>%
    huxtable::add_colnames() %>%
    fhiplot::huxtable_theme_fhi_basic() %>%
    huxtable::set_align(huxtable::everywhere, huxtable::everywhere, "center") %>%
    huxtable::set_top_padding(huxtable::everywhere, huxtable::everywhere, 0.1) %>%
    huxtable::set_bottom_padding(huxtable::everywhere, huxtable::everywhere, 0.1) %>%
    huxtable::set_left_padding(huxtable::everywhere, huxtable::everywhere, 0.1) %>%
    huxtable::set_right_padding(huxtable::everywhere, huxtable::everywhere, 0.1)

  huxtable::background_color(tab)[-1, 7] <- fhiplot::warning_color["low"]
  huxtable::background_color(tab)[which(d$status == "medium") + 1, 7] <- fhiplot::warning_color["med"]
  huxtable::background_color(tab)[which(d$status == "high") + 1, 7] <- fhiplot::warning_color["hig"]

  huxtable::background_color(tab)[which(d$status == "normal") + 1, 2] <- fhiplot::warning_color["low"]
  huxtable::background_color(tab)[which(d$status == "medium") + 1, 3] <- fhiplot::warning_color["med"]
  huxtable::background_color(tab)[which(d$status == "high") + 1, 4] <- fhiplot::warning_color["hig"]

  tab <- huxtable::add_rows(tab, tab[1, ], after = 0)

  tab <- huxtable::merge_cells(tab, 1, 2:4)
  tab[1, 2] <- "Overd\u00F8dlighet rekkevidder"

  tab <- huxtable::merge_cells(tab, 1, 5:7)
  tab[1, 5] <- "D\u00F8dsfall"

  tab[1, c(1, 8)] <- " "

  tab1_name <- "table1.png"
  tab1 <- fs::path(fhi::temp_dir(), tab1_name)
  fd::huxtable_to_png(tab, file = tab1)

  img1_name <- glue::glue("incl_reported_norge-Total-{normomo_yrwk()}.png")
  img1 <- fd::path(
    "results",
    normomo_yrwk(),
    "graphs_status",
    img1_name,
    package = "normomo"
  )

  img2_name <- glue::glue("Status_tiles-{normomo_yrwk()}.png")
  img2 <- fd::path(
    "results",
    normomo_yrwk(),
    "graphs_status",
    img2_name,
    package = "normomo"
  )

  html <- glue::glue(
    "<html>",
    "Nye NorMOMO resultater er klare. ",
    "R{fhi::nb$aa}datene kommer fra D{fhi::nb$oe}ds{fhi::nb$aa}rsaksregisteret og vi analyserer det ved bruk av <a hrc='http://github.com/euromomonetwork/momo'>MOMO</a>. ",
    "Under kan du finne overd{fhi::nb$oe}delighet sammendraget til forrige uka.<br><br><br>",
    "<b>Tabell 1.</b> Z-score (antall standardavvik) og antall registrerte d{fhi::nb$oe}dsfall de 10 siste ukene.<br><br>",
    "<img src='cid:{tab1_name}' width='800' align='middle' style='display:block;width:100%;max-width:800px' alt=''><br><br>",
    "<b>Figur 1.</b> Totalt antall d{fhi::nb$oe}dsfall per uke det siste {fhi::nb$aa}ret ({fhi::nb$oe}verst) og de siste 5 {fhi::nb$aa}rene (nederst), alle aldersgrupper.<br><br>",
    "<img src='cid:{img1_name}' width='800' align='middle' style='display:block;width:100%;max-width:800px' alt=''><br><br>",
    "<b>Figur 2.</b> Totalt antall d{fhi::nb$oe}dsfall per uke det siste {fhi::nb$aa}ret fordelt p{fhi::nb$aa} fylke.<br><br>",
    "<img src='cid:{img2_name}' width='800' align='middle' style='display:block;width:100%;max-width:800px' alt=''><br><br>",
    "</html>"
  )

  fd::mailgun(
    subject = glue::glue("NorMOMO: Uke {normomo_yrwk()} d{fhi::nb$oe}dlighet"),
    html = html,
    to = "dashboardsfhi@gmail.com",
    bcc = fd::e_emails("normomo_results"),
    inlines = c(tab1, img1, img2)
  )
}


normomo_graphs <- function() {
  fd::msg("Running normomo graphs")

  folder <- fd::path("results", normomo_yrwk(), "graphs_status", package = "normomo")
  fs::dir_create(folder)

  locs <- c("norge", unique(fhidata::norway_locations_current$county_code))

  pb <- RAWmisc::ProgressBarCreate(min = 0, max = length(locs), flush = TRUE)
  for (i in seq_along(locs)) {
    RAWmisc::ProgressBarSet(pb, i)
    loc_code <- locs[i]

    data <- fd::tbl("normomo_standard_results") %>%
      dplyr::filter(location_code == loc_code) %>%
      dplyr::collect() %>%
      fd::latin1_to_utf8()

    normomo_graphs_deaths(
      runName = loc_code,
      data = data,
      folder = folder
    )
  }

  normomo_tiles_fylker(folder)
}

normomo_graphs_deaths <- function(
                                  runName = "norge",
                                  data,
                                  folder) {
  storedData <- list()
  if (runName == "norge") {
    runList <- c("Total", "0to4", "5to14", "15to64", "65P")
  } else {
    runList <- "Total"
  }
  for (i in runList) {
    if (i == "Total") {
      title1 <- "Totalt antall d\u00F8de per uke siste \u00E5r"
      title1a <- "Totalt antall d\u00F8de per uke siste \u00E5r (med rapporterte d\u00F8dsfall)"
      title1b <- "Totalt antall d\u00F8de per uke siste \u00E5r (uten rapporterte d\u00F8dsfall)"
      title2 <- "Totalt antall d\u00F8de per uke siste 5 \u00E5r"
      titleBias <- "Bias i korrigering av totalt antall d\u00F8de per uke siste"
    } else if (i == "0to4") {
      title1 <- "Antall d\u00F8de (0-4 \u00E5r) per uke siste \u00E5r"
      title1a <- "Antall d\u00F8de (0-4 \u00E5r) per uke siste \u00E5r (med rapporterte d\u00F8dsfall)"
      title1b <- "Antall d\u00F8de (0-4 \u00E5r) per uke siste \u00E5r (uten rapporterte d\u00F8dsfall)"
      title2 <- "Antall d\u00F8de (0-4 \u00E5r) per uke siste 5 \u00E5r"
      titleBias <- "Bias i korrigering av antall d\u00F8de (0-4 \u00E5r) per uke"
    } else if (i == "5to14") {
      title1 <- "Antall (5-14 \u00E5r) d\u00F8de per uke siste \u00E5r"
      title1a <- "Antall (5-14 \u00E5r) d\u00F8de per uke siste \u00E5r (med rapporterte d\u00F8dsfall)"
      title1b <- "Antall (5-14 \u00E5r) d\u00F8de per uke siste \u00E5r (uten rapporterte d\u00F8dsfall)"
      title2 <- "Antall (5-14 \u00E5r) d\u00F8de per uke siste 5 \u00E5r"
      titleBias <- "Bias i korrigering av antall d\u00F8de (5-14 \u00E5r) per uke"
    } else if (i == "15to64") {
      title1 <- "Antall (15-64 \u00E5r) d\u00F8de per uke siste \u00E5r"
      title1a <- "Antall (15-64 \u00E5r) d\u00F8de per uke siste \u00E5r (med rapporterte d\u00F8dsfall)"
      title1b <- "Antall (15-64 \u00E5r) d\u00F8de per uke siste \u00E5r (uten rapporterte d\u00F8dsfall)"
      title2 <- "Antall (15-64 \u00E5r) d\u00F8de per uke siste 5 \u00E5r"
      titleBias <- "Bias i korrigering av antall d\u00F8de (15-64 \u00E5r) per uke"
    } else if (i == "65P") {
      title1 <- "Antall (65+ \u00E5r) d\u00F8de per uke siste \u00E5r"
      title1a <- "Antall (65+ \u00E5r) d\u00F8de per uke siste \u00E5r (med rapporterte d\u00F8dsfall)"
      title1b <- "Antall (65+ \u00E5r) d\u00F8de per uke siste \u00E5r (uten rapporterte d\u00F8dsfall)"
      title2 <- "Antall (65+ \u00E5r) d\u00F8de per uke siste 5 \u00E5r"
      titleBias <- "Bias i korrigering av antall d\u00F8de (65+ \u00E5r) per uke"
    }

    q <- gridExtra::grid.arrange(
      GraphTogether(
        data = data[age == i],
        title1 = title1,
        title2 = title2,
        includeRealDeaths = FALSE
      ),
      ncol = 1,
      newpage = F,
      bottom = RAWmisc::FootnoteGridArrange(paste("Sist oppdatert: ", strftime(fd::get_rundate()[package == "normomo"]$date_extraction, format = "%d/%m/%Y"), sep = ""))
    )
    fhiplot::save_a4(q, filename = paste0(folder, "/excl_reported_", runName, "-", i, "-", normomo_yrwk(), ".png"))

    q <- gridExtra::grid.arrange(
      GraphTogether(
        data = data[age == i],
        title1 = title1,
        title2 = title2,
        includeRealDeaths = TRUE
      ),
      ncol = 1,
      newpage = F,
      bottom = RAWmisc::FootnoteGridArrange(paste("Sist oppdatert: ", strftime(fd::get_rundate()[package == "normomo"]$date_extraction, format = "%d/%m/%Y"), sep = ""))
    )
    fhiplot::save_a4(q, filename = paste0(folder, "/incl_reported_", runName, "-", i, "-", normomo_yrwk(), ".png"))
  }
}


normomo_tiles_fylker <- function(folder) {
  allResults <- fd::tbl("normomo_standard_results") %>%
    dplyr::filter(age == "Total") %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()

  x_yrwk <- rev(sort(as.character(unique(allResults$yrwk))))[1:52]
  plotData <- allResults[yrwk %in% x_yrwk & age == "Total"]
  plotData[, status := "1veryhigh"]
  plotData[nbc < UPIb4, status := "2high"]
  plotData[nbc < UPIb2, status := "3expected"]

  plotData[fhidata::norway_locations_long_current, on = "location_code", location_name := location_name]
  plotData <- plotData[!is.na(location_name)]
  unique(plotData$location_code)
  unique(plotData$location_name)


  plotData[, location_name := factor(location_name, levels = rev(fhidata::norway_locations_long_current[location_code %in% plotData$location_code]$location_name))]

  plotColours <- plotData[1:4]
  # plotColours[1,status:="4lower"]
  plotColours[2, status := "3expected"]
  plotColours[3, status := "2high"]
  plotColours[4, status := "1veryhigh"]

  q <- ggplot(plotData, aes(x = yrwk, y = location_name, fill = status))
  q <- q + geom_tile(colour = "black")
  q <- q + geom_tile(data = plotColours, alpha = 0)
  q <- q + scale_fill_manual("",
    values = c("1veryhigh" = fhiplot::warning_color[["hig"]], "2high" = fhiplot::warning_color[["med"]], "3expected" = fhiplot::warning_color[["low"]]),
    labels = c(
      "Betydelig h\u00F8yere enn forventet",
      "H\u00F8yere enn forventet",
      "Forventet/lavere enn forventet"
    )
  )
  q <- q + labs(title = "Totalt antall d\u00F8de per uke siste \u00E5r")
  q <- q + scale_x_discrete("\u00C5r-uke", expand = c(0, 0))
  q <- q + scale_y_discrete("", expand = c(0, 0))
  q <- q + labs(caption = sprintf("Sist oppdatert: %s", strftime(fd::get_rundate()[package == "normomo"]$date_extraction, format = "%d/%m/%Y")))
  q <- q + fhiplot::theme_fhi_basic()
  q <- q + fhiplot::set_x_axis_vertical()
  # q
  fhiplot::save_a4(
    q,
    fs::path(folder, glue::glue("Status_tiles-{normomo_yrwk()}.png")),
    landscape = T
  )
}


GraphTogether <- function(
                          data,
                          norwegian = TRUE,
                          title1 = NULL,
                          title1a = NULL,
                          title1b = NULL,
                          title2,
                          includeRealDeaths = FALSE) {
  if (!is.null(title1)) {
    plottingData1 <- data[wk >= max(wk) - 52]
    plottingData2 <- data[wk >= max(wk) - 52 * 5 + 1]

    plottingData1[, titlex := title1]
    plottingData2[, titlex := title2]

    plottingData1[, type := "top"]
    plottingData2[, type := "bottom"]

    plottingData <- rbind(plottingData1, plottingData2)
    plottingData[, titlex := factor(titlex, levels = c(title1, title2))]
  } else {
    plottingData1a <- data[wk >= max(wk) - 52]
    plottingData1b <- data[wk >= max(wk) - 52]
    plottingData2 <- data[wk >= max(wk) - 52 * 5 + 1]

    plottingData1a[, titlex := title1a]
    plottingData1b[, titlex := title1b]
    plottingData2[, titlex := title2]

    plottingData1a[, type := "top"]
    plottingData1b[, type := "top"]
    plottingData2[, type := "bottom"]

    plottingData <- rbind(plottingData1a, plottingData1b, plottingData2)
    plottingData[, titlex := factor(titlex, levels = c(title1a, title1b, title2))]
  }

  plottingData[, ymax := max(nbc, UPIb4)]
  plottingData[, ymin := min(nbc, UPIb4)]
  plottingData[, Lower := Pnb - abs(UPIb2 - Pnb)]
  plottingData[Lower < 0, Lower := 0]
  plottingData[, unstableEstimates := "Stable"]
  plottingData[wk >= max(wk) - 7, unstableEstimates := "Unstable"]

  plottingData[, wkSplit := wk]
  plottingData[type == "bottom", wkSplit := wk * 10]

  breaks <- unique(plottingData[, c("WoDi", "YoDi", "wk"), with = FALSE])
  breaksTop <- breaks[seq(1, 53, 4)]
  breaksTop[, label := paste(gsub(" ", "0", format(WoDi, width = 2)), "/", YoDi, sep = "")]

  breaks <- unique(plottingData[, c("wk", "YoDi"), with = FALSE])
  setorder(breaks, wk)
  breaks[, YoDi2 := shift(YoDi)]
  breaksBottom <- stats::na.omit(breaks[breaks$YoDi != breaks$YoDi2, ])
  breaksBottom$label <- paste("1/", breaksBottom$YoDi, sep = "")
  breaksBottom[, wk := wk * 10]

  breaks <- rbind(breaksTop[, c("wk", "label")], breaksBottom[, c("wk", "label")])

  if (norwegian) {
    filllabels1 <- c("Prediksjonsintervall", "Betydelig h\u00F8yere enn forventet", "H\u00F8yere enn forventet", "Forventet", "Lavere enn forventet")
    shapelabels <- c("Forel\u00F8pig")
    colourlabels <- c("Korrigert for forsinkelse", "Rapporterte d\u00F8dsfall")
    ylabel <- "Antall d\u00F8de per uke"
  } else {
    filllabels1 <- c("Prediction interval", "Significantly higher than expected", "Higher than expected", "Expected", "Lower than expected")
    filllabels2 <- c("Prediction interval", "Higher than expected", "Expected", "Lower than expected")
    shapelabels <- c("Preliminary numbers")
    colourlabels <- c("Corrected for delays", "Reported deaths")
    ylabel <- "Deaths per week"
  }

  q <- ggplot(plottingData, aes(x = wkSplit))
  q <- q + geom_ribbon(aes(ymin = -Inf, ymax = Lower, fill = "5lower"))
  q <- q + geom_ribbon(aes(ymin = Lower, ymax = UPIb2, fill = "4expected"))
  q <- q + geom_ribbon(aes(ymin = UPIb2, ymax = UPIb4, fill = "3high"))
  q <- q + geom_ribbon(aes(ymin = UPIb4, ymax = Inf, fill = "2veryhigh"))
  q <- q + geom_ribbon(data = plottingData[unstableEstimates == "Unstable" & type == "top"], mapping = aes(ymin = LPIc, ymax = UPIc, fill = "1predinterval"), alpha = 0.3)
  if (includeRealDeaths) q <- q + geom_line(data = plottingData[titlex %in% c(title1, title1a)], mapping = aes(y = nb, colour = "Rapporterte"), lwd = 0.5)
  q <- q + geom_line(aes(y = nbc, colour = "Korrigert"), lwd = 0.5)
  q <- q + geom_point(data = plottingData[unstableEstimates == "Unstable"], aes(y = nbc, shape = "Usikkert"), size = 2)
  q <- q + facet_wrap(~titlex, scales = "free", ncol = 1)
  # q <- q + labs(title=title)
  q <- q + scale_x_continuous("", breaks = breaks$wk, labels = breaks$label, expand = expand_scale(mult = c(0, 0.01)))
  q <- q + scale_y_continuous(ylabel)
  q <- q + scale_fill_manual("",
    values = c(
      "1predinterval" = "#636363",
      "2veryhigh" = fhiplot::warning_color[["hig"]],
      "3high" = fhiplot::warning_color[["med"]],
      "4expected" = fhiplot::warning_color[["low"]],
      "5lower" = "white"
    ),
    labels = filllabels1
  )
  q <- q + scale_shape_manual("",
    values = c("Usikkert" = 16),
    labels = shapelabels
  )
  q <- q + scale_colour_manual("",
    values = c("Korrigert" = "black", "Rapporterte" = "red"),
    labels = colourlabels
  )
  q <- q + fhiplot::theme_fhi_lines(base_size = 18)
  q <- q + fhiplot::set_x_axis_vertical()
  # q <- q + theme(panel.grid.major = element_line(colour = "white"),
  #               panel.grid.minor = element_line(colour = "white", size = 0.25))
  q <- q + guides(fill = guide_legend(title.position = "top", reverse = F, order = 1, ncol = 1))
  q <- q + guides(colour = guide_legend(title.position = "top", reverse = F, order = 2, ncol = 1))
  q <- q + guides(shape = guide_legend(title.position = "top", reverse = F, order = 3, ncol = 1))

  if (!is.null(title1)) {
    q <- q + theme(legend.position = "right")
  } else {
    q <- q + theme(legend.position = "bottom")
  }
  # q <- SMAOFormatGGPlot(q, legendPos="right", xAngle=90,ncol=1,legendBorder=TRUE)
  # q <- format_plot(q,2,2,stripes=TRUE, xangle=90)
  return(q)
}
