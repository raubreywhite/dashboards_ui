signalsUI <- function(id, label = "Counter", GLOBAL) {
  ns <- NS(id)
  fluidRow(
    column(
      width=2,
      selectInput(ns("weeklyOutbreakWeek"), "Uker", as.list(GLOBAL$outbreaksyrwk), selected = GLOBAL$outbreaksyrwk[1]),
      selectInput(ns("weeklyOutbreakSort"), "Rangere etter", list("Z verdi"="zscore","Eksess tilfeller"="cases","Navn"="none"), selected = "zscore"),
      checkboxInput(ns("weeklyOutbreakHideEmpty"), "Skjul tomme", TRUE)
    ),
    column(
      width=10,
      tabBox(
        width=12,
        tabPanel(
          title="Fylker",
          box(
            width=12,
            tableOutput(ns("table1"))
          )
        ),
        tabPanel(
          title="Kommuner",
          box(
            width=12,
            tableOutput(ns("table2"))
          )
        ),
        tabPanel(
          title="Info",
          box(
            width=12,
            p("Tabellen viser en oversikt over forekomsten av sykdom/symptom i et valgt tidsrom."),
            p("Valg av tidsrom gjøres på bunnefiguren. Valg av sykdom/symptom gjøres på venstre side. På venstre side kan man også velge Norge eller et fylke i Norge. Hvis man velger Norge vil hvert fylke få en rad i tabellen. Hvis man velger et fylke vil alle kommunene få en rad i tabellen."),
            p("Dersom ruten for en gitt uke er farget med grønn farge betyr det at antall konsultasjoner i den gitte kommunen eller fylket er som forventet denne uken. En gul farge en gitt uke betyr at antall konsultasjoner i den gitte kommunen eller fylket er høyere enn forventet denne uken. En rød farge en gitt uke betyr at antall konsultasjoner i den gitte kommunen eller fylket er betydelig høyere enn forventet denne uken. Fargene er laget ut fra beregninger fra de foregående 5 årene."),
            p("Se punktet *Om Sykdomspulsen* på venstre side for mer utfyllende informasjon om dataene og beregninger.")
          )
        )
      )
    )
  )
}

signalsServer <- function(input, output, session, GLOBAL) {
  output$table1 <- renderTable({
    x_wkyr <- input$weeklyOutbreakWeek

    data_county <- pool %>% tbl("spuls_standard_results") %>%
      filter(tag != "influensa" &
               granularity_time == "weekly" &
               granularity_geo == "county" &
               yrwk==x_wkyr) %>% collect()
    data_municipality <- pool %>% tbl("spuls_standard_results") %>%
      filter(tag != "influensa" &
               granularity_time == "weekly" &
               granularity_geo == "municip" &
               yrwk==x_wkyr) %>% collect()
    setDT(data_county)
    setDT(data_municipality)
    data <- GenerateOutbreakListInternal(df=data_county,
                                         dk=data_municipality, saveFiles=NULL)
    data <- data[["df"]]

    if(input$weeklyOutbreakHideEmpty){
      data <- data[data$High!="",]
    }
    if(input$weeklyOutbreakSort=="zscore"){
      setorder(data,-meanZScore)
    } else if(input$weeklyOutbreakSort=="cases"){
      setorder(data,-sumCum)
    }
    if(nrow(data)==0) return(data.frame("Obs"="Ingen utbrudd denne uken"))
    data$yrwk <- NULL
    data$sumCum[is.na(data$sumCum)] <- 0
    data$sumCum <- formatC(data$sumCum,digits=0,format="f")
    data$sumCum[data$sumCum=="0"] <- ""
    setnames(data,c("Sykdom","Alder","Fylke (Z verdi)","Gj. Z Verdi","Eksess tilfeller"))
    data$Sykdom <- factor(data$Sykdom,levels=sykdomspuls::CONFIG$STANDARD$tag)
    levels(data$Sykdom) <- sykdomspuls::CONFIG$STANDARD$namesLong
    data
  },
  striped=TRUE,
  spacing="m",
  align="c",
  width='100%')

  output$table2 <- renderTable({
    x_wkyr <- input$weeklyOutbreakWeek

    data_county <- pool %>% tbl("spuls_standard_results") %>%
      filter(tag != "influensa" &
               granularity_time == "weekly" &
               granularity_geo == "county" &
               yrwk==x_wkyr) %>% collect()
    data_municipality <- pool %>% tbl("spuls_standard_results") %>%
      filter(tag != "influensa" &
               granularity_time == "weekly" &
               granularity_geo == "municip" &
               yrwk==x_wkyr) %>% collect()
    setDT(data_county)
    setDT(data_municipality)
    data <- GenerateOutbreakListInternal(df=data_county,
                                         dk=data_municipality, saveFiles=NULL)
    data <- data[["dk"]]
    if(input$weeklyOutbreakHideEmpty){
      data <- data[data$High!="",]
    }
    if(input$weeklyOutbreakSort=="zscore"){
      setorder(data,-meanZScore)
    } else if(input$weeklyOutbreakSort=="cases"){
      setorder(data,-sumCum)
    }

    if(nrow(data)==0) return(data.frame("Obs"="Ingen utbrudd denne uken"))
    data$yrwk <- NULL
    data$sumCum[is.na(data$sumCum)] <- 0
    data$sumCum <- formatC(data$sumCum,digits=0,format="f")
    data$sumCum[data$sumCum=="0"] <- ""
    setnames(data,c("Sykdom","Alder","Fylke","Kommune (Z verdi)","Gj. Z Verdi","Eksess tilfeller"))
    data$Sykdom <- factor(data$Sykdom,levels=sykdomspuls::CONFIG$STANDARD$tag)
    levels(data$Sykdom) <- sykdomspuls::CONFIG$STANDARD$namesLong
    data
  },
  striped=TRUE,
  spacing="m",
  align="c",
  width='100%')
}
