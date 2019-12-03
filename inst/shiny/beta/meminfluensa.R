meminfluensaUI <- function(id, label = "Counter", GLOBAL) {
  ns <- NS(id)
  fluidRow(
    column(
      width=2,
      selectInput(ns("influensaCounty"), "Fylke", as.list(GLOBAL$weeklyCounties), selected = GLOBAL$weeklyCounties[1]),
      uiOutput(ns("influensaSeason"))
    ),
    column(
      width=10,
      tabBox(
        width=12,
        tabPanel(
          title="Figur",
          box(plotOutput(ns("influensa_plot"), height="100%"), width=13, style='height:80vh')
        ),
        tabPanel(
          title="Info",
          p("Info")
        )
      )
    )
  )
}

meminfluensaServer <- function(input, output, session, GLOBAL) {
  current_season <- fhi::season(GLOBAL$outbreaksyrwk[1], start_week=30)
  seasons <- pool %>%
    tbl("spuls_mem_results") %>%
    filter(tag == "influensa" & !is.na(low)) %>%
    distinct(season) %>% arrange(desc(season)) %>%
    collect()

  output$influensaSeason <- renderUI(selectInput(session$ns("influensaSeason"), "Sesong", as.list(seasons)$season, selected = current_season))

  influensa_data <- reactive({
    req(input$influensaCounty)
    req(input$influensaSeason)

    x_location <- input$influensaCounty
    x_season <- input$influensaSeason
    data <- pool %>%
      tbl("spuls_mem_results") %>%
      filter(season == x_season &
               location_code == x_location &
               tag == "influensa") %>%
      collect()
    setDT(data)
    data[, granularity_time:="weekly"]
    data <- calculate_confidence_interval(data, last_weeks=10)
    return(data)
  })

  output$influensa_plot <- renderPlot({
    data <- influensa_data()
    t2 <- Getlocation_name(input$influensaCounty)
    title<- paste0("Prosent pasienter med influensa i ", t2)
    q <- fhiplot::make_influenza_threshold_chart(data, title)
    return(q)

  })
}
