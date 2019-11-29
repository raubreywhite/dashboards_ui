fhi::DashboardInitialiseOpinionated("sykdomspuls")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(shiny))
library(flexdashboard)
library(ggplot2)
library(ggrepel)
library(data.table)
library(fhi)
library(magrittr)
library(dplyr)

options(shiny.port = 4989)
options(shiny.host = "0.0.0.0")

file <- system.file("shiny",
  "sykdomspuls",
  "global.R",
  package = "sykdomspuls"
)
source(file, .GlobalEnv)

file <- system.file("shiny",
  "sykdomspuls",
  "flexdashboard.Rmd",
  package = "sykdomspuls"
)

rmarkdown::run(file, shiny_args = list(port = 4989))
