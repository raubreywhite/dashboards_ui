overview_ui <- function(id, config) {
  ns <- NS(id)
  fluidRow(
    column(
      width=2,
      selectInput(ns("overview_age"), "Alder", as.list(config$ages), selected = config$ages[1])
    ),
    column(
      width=10,
      tabsetPanel(
        tabPanel(title="Figur",
          br(),
          div(style='height:80vh;text-align: center',plotOutput(ns("overview_plot"), height="100%"))
        ),
        tabPanel(
          title="Info",
          br()
        )
      )
    )
  )
}

overview_server <- function(input, output, session, config) {
  start_date <- config$start_date

  overview_data <- reactive({
    req(input$overview_age)

    x_age <- input$overview_age
    retData <- pool %>% tbl("spuls_standard_results") %>%
      filter(
        date >= start_date &
        tag == "corona" &
        location_code=="norge" &
        granularity_time =="daily" &
        age== x_age
      ) %>% collect()
    setDT(retData)
    return(retData)
  })

  output$overview_plot <- renderCachedPlot({
    pd <- overview_data()

    pd <- pd[,c("date","n"),with=F]
    q <- ggplot(pd, aes(x=date,y=n))
    q <- q + geom_line()
    q
  }, cacheKeyExpr={list(
    lubridate::today(),
    input$overview_age
  )})
}
