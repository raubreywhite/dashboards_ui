## app.R ##
library(shiny)
library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)

shinyOptions(cache = diskCache("/tmp/", max_size = 50e6))

source("global.R")
source("overview.R")

ui <- navbarPage("Sykdomspulsen/COVID-19",
  tabPanel("Oversikt",
    overview_ui("overview", config=config)
  )
)

server <- function(input, output) {
  callModule(overview_server, "overview", config=config)
}

shinyApp(ui, server)

#  shiny::runApp('inst/shiny/corona', port = 4989, host = "0.0.0.0")
