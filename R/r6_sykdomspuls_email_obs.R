#' normomo
#' @import R6
#' @export sykdomspuls_obs
sykdomspuls_obs <- R6::R6Class(
  "sykdomspuls_obs",
  portable = FALSE,
  cloneable = FALSE,
  list(
    run_all = function() {
      # check to see if it can run
      rundate <- fd::get_rundate()
      run <- TRUE
      if ("ui_sykdomspuls_obs" %in% rundate$package) {
        if (rundate[package == "ui_sykdomspuls_obs"]$date_extraction >= rundate[package == "sykdomspuls"]$date_extraction) run <- FALSE
      }
      if (!run & fd::config$is_production) {
        return()
      }

      if(actions[["sykdomspuls_obs"]]$can_perform_action()){
        sykdomspuls_obs_email_external()
        actions[["sykdomspuls_obs"]]$action_performed()
      }

      # update rundate
      fd::update_rundate(
        package = "ui_sykdomspuls_obs",
        date_extraction = rundate[package == "sykdomspuls"]$date_extraction,
        date_results = rundate[package == "sykdomspuls"]$date_results,
        date_run = lubridate::today()
      )
    }
  )
)


EmailExternalGenerateTable <- function(results, xtag) {
  r <- results[tag == xtag]
  setorder(r, tag, -zscore)

  if (nrow(r) == 0) {
    return(sprintf("%s utbrudd:<br><br>Ingen utbrudd registrert", sykdomspuls::CONFIG$SYNDROMES[tag == xtag]$namesLong))
  }

  tab <- huxtable::huxtable(
    Syndrom = r$tag_pretty,
    "Geografisk omr\u00E5de" = r$link,
    Alder = r$age,
    `Meldte<br>tilfeller` = r$n,
    `Flere tilfeller<br>enn forventet` = r$cumE1,
    `Z-verdi` = r$zscore
  ) %>%
    huxtable::add_colnames() %>%
    fhiplot::huxtable_theme_fhi_basic()
  huxtable::escape_contents(tab)[, 2] <- FALSE
  huxtable::escape_contents(tab)[1, 4:5] <- FALSE
  huxtable::number_format(tab)[, 5] <- 0
  huxtable::number_format(tab)[, 6] <- 1
  huxtable::background_color(tab)[-1, 6] <- "yellow"
  huxtable::background_color(tab)[which(r$zscore >= 4) + 1, 6] <- "red"
  huxtable::align(tab)[1, ] <- "center"
  huxtable::align(tab)[-1, 3:6] <- "center"

  return(huxtable::to_html(tab))
}

#' Sends an external email warning about alters
sykdomspuls_obs_email_external <- function() {
  max_yrwk <- fhi::isoyearweek(fd::get_rundate()[package == "sykdomspuls"]$date_results)
  tag_relevant <- sykdomspuls::CONFIG$MODELS$standard[alertExternal == T]$tag

  results <- fd::tbl("spuls_standard_results") %>%
    dplyr::filter(granularity_time == "weekly") %>%
    dplyr::filter(yrwk == !!max_yrwk) %>%
    dplyr::filter(status != "Normal") %>%
    dplyr::filter(tag %in% !!tag_relevant) %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()

  alerts <- sykdomspuls_obs_get_emails()
  setDT(alerts)
  emails <- unique(alerts$email)


  emailSubjectNoOutbreak <- "Pilotprosjektet Sykdomspulsen til kommunehelsetjenesten er oppdatert med nye tall"
  emailSubjectYesOutbreak <- "OBS varsel fra Sykdomspulsen"

  if (!fd::config$is_production) {
    if (!"test@rwhite.no" %in% unique(alerts$email)) stop("THIS IS NOT A TEST EMAIL DATASET")
  }

  emailNoOutbreak <-
    "Pilotprosjektet Sykdomspulsen til kommunehelsetjenesten er oppdatert med nye tall.<br>
  Nye resultater vises p\u00E5 websiden om ca. 10 min.<br><br>
  Innlogging<br>
  Webadresse: <a href='http://sykdomspulsen.fhi.no/lege123/'>http://sykdomspulsen.fhi.no/lege123/</a><br>
  Det er ikke noe brukernavn eller passord, du kommer direkte inn p\u00E5 nettsiden og den er klar til bruk.
  Bruk Google Chrome n\u00E5r du logger deg inn.<br><br>
  NB! Dette er et pilotprosjekt. Du f\u00E5r n\u00E5 mulighet til \u00E5 bruke websiden n\u00E5r du vil og s\u00E5 mye du vil.
  Du kan ogs\u00E5 vise enkeltsider av websiden til andre som jobber innenfor kommunehelsetjenesten.
  MEN vi ber om at du ikke distribuerer webadressen til andre, hverken til ansatte i kommunehelsetjenesten eller utenfor.
  Det er fordi dette er et pilotprosjekt der vi \u00F8nsker \u00E5 ha oversikt over hvem som bruker systemet.
  Dersom noen andre enn deg \u00F8nsker \u00E5 f\u00E5 tilgang til websiden kan de kontakte oss p\u00E5 sykdomspulsen@fhi.no<br><br>
  NB! Fra n\u00E5 vil det bli sendt ut OBS varsel fra Sykdomspulsen.<br><br>
  OBS varselet inneb\u00E6rer at du vil f\u00E5 en mail dersom deres kommune eller fylke har flere konsultasjoner enn forventet av henholdsvis mage-tarminfeksjoner eller luftveisinfeksjoner sist uke.<br><br>
  E-posten vil ha overskrift <b>OBS varsel fra Sykdomspulsen uke xx</b>.<br><br>
  E-posten vil inneholde en tabell med informasjon om stedet der det er mer enn forventet antall konsultasjoner med aldersgruppe, antallet konsultasjoner som er over forventet verdi (excess) og en verdi som viser hvor ekstremt signalet er (z-score).<br><br>
  Varselet er en informasjon om at det kan v\u00E6re noe som b\u00F8r f\u00F8lges opp i deres kommune eller i et fylke. Det anbefales \u00E5 g\u00E5 inn i Sykdomspulsen websiden og sjekke det ut. Varselet beh\u00F8ver ikke \u00E5 bety noe alvorlig.<br><br>
  Nederst i denne mailen viser vi hvilke(n) kommune(r) du f\u00E5r varsel for. Alle f\u00E5r varsel for alle fylker og hele Norge. Dersom det ikke st\u00E5r noen kommune i tabellen mangler vi det for deg og ber deg kontakte oss for \u00E5 f\u00E5 satt opp riktig kommune(r).<br><br>
  Send oss gjerne en tilbakemelding dersom du \u00F8nsker varsel for andre kommuner. Vi \u00F8nsker ogs\u00E5 tilbakemelding p\u00E5 om dette varselet er nyttig for dere eller ikke p\u00E5 sykdomspulsen@fhi.no<br><br>
  Nord og S\u00F8r-Tr\u00F8ndelag har fra 01.01.2018 blitt sl\u00E5tt sammen til Tr\u00F8ndelag. Det vil derfor bare v\u00E6re mulig \u00E5 finne Tr\u00F8ndelag i nedtrekks listen for Fylkene.<br><br>
  Sykdomspulsen kan i noen tilfeller generere et OBS varsel selv om det bare er en eller to konsultasjoner for et symptom/sykdom. Dette sees som oftest i sm\u00E5 kommuner der det vanligvis ikke er mange konsultasjoner. For ikke \u00E5 bli forstyrret av slike signaler har vi n\u00E5 lagt inn en nedre grense for gult signal p\u00E5 to konsultasjoner og en nedre grense for r\u00F8dt signal p\u00E5 tre konsultasjoner.<br><br>
  Dersom du har problemer med websiden, forslag til forbedringer, ris eller ros kan du sende oss en mail: sykdomspulsen@fhi.no<br><br> <br>
  Dersom du ikke \u00F8nsker \u00E5 f\u00E5 denne e-posten n\u00E5r vi oppdaterer Sykdomspulsen med nye tall s\u00E5 kan du gi oss beskjed ved \u00E5 sende en mail til adressen over.<br><br>
  <b> NB! Oppdatering av Sykdomspulsen vil n\u00E5 skje p\u00E5 onsdager istedenfor tirsdager. H\u00E5per dette ikke vil for\u00E5rsake noen ulemper for dere.</b> <br><br>
  Hilsen:<br>
  Sykdomspulsen ved Folkehelseinstituttet<br>
  v/Gry M Gr\u00F8neng (prosjektleder) og Richard White (statistiker og webansvarlig)<br><br><br>
  "

  emailYesOutbreak <-
    "Dette er et OBS varsel fra Sykdomspulsen.<br><br>
  OBS varselet inneb\u00E6rer at alle dere som deltar i pilotprosjektet <b>Sykdomspulsen til kommunehelsetjenesten</b> f\u00E5r et varsel p\u00E5 e-post dersom deres kommune eller et fylke har flere konsultasjoner enn forventet av henholdsvis mage-tarminfeksjoner eller luftveisinfeksjoner sist uke.<br><br>
  Tabellen under viser informasjon om stedet der det er mer enn forventet antall konsultasjoner og aldersgruppe, antallet konsultasjoner som er over forventet verdi (excess) og en verdi som viser hvor ekstremt signalet er (z-score). Hvis z-scoret er mellom 2 og 4 er antallet konsultasjoner sist uke h\u00F8yere enn forventet og man vil se at det ligger i gul sone p\u00E5 Sykdomspulsen websiden. Dersom z-scoret er over 4 er antallet konsultasjoner sist uke betydelig h\u00F8yere enn forventet og man vil se at det ligger i r\u00F8d sone p\u00E5 Sykdomspulsen websiden.<br><br>
  I tabellen under er det en link til stedet der du kan se OBS varselet i Sykdomspulsen. Denne virker ikke dersom den \u00E5pnes i Internet explorer. Dersom du har problemer med linken kan du h\u00F8yreklikke p\u00E5 koblingen og kopiere den for deretter \u00E5 lime den inn i for eksempel Google chrome eller en annen nettleser. Du kan ogs\u00E5 logge deg inn p\u00E5 Sykdomspulsen p\u00E5 vanlig m\u00E5te (<a href='http://sykdomspulsen.fhi.no/lege123/'>http://sykdomspulsen.fhi.no/lege123/</a>) og selv finne aktuell kommune eller fylke.<br><br>
  Varselet er en informasjon om at det kan v\u00E6re noe som b\u00F8r f\u00F8lges opp i deres kommune eller i et fylke. Det anbefales \u00E5 g\u00E5 inn i Sykdomspulsen websiden og sjekke det ut. Varselet beh\u00F8ver ikke \u00E5 bety noe alvorlig.<br><br>
  Nederst i denne mailen viser vi hvilke(n) kommune(r) du f\u00E5r varsel for. Alle f\u00E5r varsel for alle fylker og hele Norge. Dersom det ikke st\u00E5r noen kommune i tabellen mangler vi det for deg og ber deg kontakte oss for \u00E5 f\u00E5 satt opp riktig kommune(r).<br><br>
  Sykdomspulsen kan i noen tilfeller generere et OBS varsel selv om det bare er en eller to konsultasjoner for et symptom/sykdom. Dette sees som oftest i sm\u00E5 kommuner der det vanligvis ikke er mange konsultasjoner. For ikke \u00E5 bli forstyrret av slike signaler har vi n\u00E5 lagt inn en nedre grense for gult signal p\u00E5 to konsultasjoner og en nedre grense for r\u00F8dt signal p\u00E5 tre konsultasjoner.<br><br>
  Ta kontakt med oss om du har sp\u00F8rsm\u00E5l eller om det er noe som er uklart p\u00E5 sykdomspulsen@fhi.no.<br><br>
  Send oss ogs\u00E5 en tilbakemelding dersom du \u00F8nsker varsel for andre kommuner eller fylker.<br><br>
  Vi \u00F8nsker ogs\u00E5 tilbakemelding p\u00E5 om dette varselet er nyttig for dere eller ikke.<br><br>
  <b> NB! Oppdatering av Sykdomspulsen vil n\u00E5 skje p\u00E5 onsdager istedenfor tirsdager. H\u00E5per dette ikke vil for\u00E5rsake noen ulemper for dere.</b> <br><br>
  Hilsen:<br><br>
  Sykdomspulsen ved Folkehelseinstituttet<br>
  v/Gry M Gr\u00F8neng (prosjektleder) og Richard White (statistiker og webansvarlig)<br><br>
  "

  alerts[, output := sprintf("<tr> <td>%s</td> </tr>", location)]

  setorder(results, -zscore)
  results[, tag_pretty := tag]
  RAWmisc::RecodeDT(results, switch = sykdomspuls::CONFIG$tagsWithLong, var = "tag_pretty", oldOnLeft = FALSE)
  results[, link := sprintf("<a href='http://sykdomspulsen.fhi.no/lege123/#/ukentlig/%s/%s/%s/%s'>%s</a>", county_code, location_code, tag, age, location_name)]
  results[is.na(county_code), link := sprintf("<a href='http://sykdomspulsen.fhi.no/lege123/#/ukentlig/%s/%s/%s/%s'>%s</a>", location_code, location_code, tag, age, location_name)]


  for (em in emails) {
    a <- alerts[email %in% em]

    r <- vector("list", length = nrow(a))
    for (i in 1:nrow(a)) {
      r[[i]] <- results[stringr::str_detect(location_code, a$location[i]) & status %in% a$statuses[[i]]]
    }
    r <- rbindlist(r)


    noOutbreak <- nrow(r) == 0

    # no outbreaks
    if (noOutbreak) {
      emailText <- emailNoOutbreak
      emailSubject <- emailSubjectNoOutbreak
      useEmail <- "xxxxxxx"
    } else {
      emailText <- emailYesOutbreak
      emailSubject <- emailSubjectYesOutbreak
      useEmail <- em
    }

    # include registered places
    tab <- huxtable::huxtable("Geografisk omr\u00E5de" = a$location) %>%
      huxtable::add_colnames() %>%
      fhiplot::huxtable_theme_fhi_basic()
    huxtable::escape_contents(tab)[1, 1] <- FALSE
    tab <- huxtable::to_html(tab)

    emailText <- paste0(emailText, "Du er registrert for \u00E5 motta varsel om utbrudd i:<br>", tab, "<br><br>")

    # include outbreaks
    for (tag in sykdomspuls::CONFIG$SYNDROMES[alertExternal == T]$tag) {
      emailText <- paste0(emailText, EmailExternalGenerateTable(results = r, xtag = tag), "<br>")
    }

    fd::mailgun(
      subject = emailSubject,
      html = emailText,
      to = em,
      is_final = actions[["sykdomspuls_obs"]]$is_final()
    )

    Sys.sleep(1)
  }

  return(0)
}

AlertsEmailConverter <- function(emails) {
  setDT(emails)
  emails[, statuses := vector("list", length = .N)]
  emails[, statuses := rep(list(c("High", "Medium")), .N)]
  emails[level == "high", statuses := rep(list(c("High")), .N)]

  return(emails)
}


sykdomspuls_obs_get_emails <- function() {
  if (fd::config$is_production & actions[["sykdomspuls_obs"]]$is_final()) {
    retval <- readxl::read_excel(file.path("/etc", "gmailr", "emails_sykdomspuls_alert.xlsx"))
  } else {
    retval <- readxl::read_excel(file.path("/etc", "gmailr", "emails_sykdomspuls_alert_test.xlsx"))
  }

  retval <- AlertsEmailConverter(retval)

  return(retval)
}

