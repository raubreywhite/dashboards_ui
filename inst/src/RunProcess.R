fd::initialize("ui")
suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
options(bitmapType = 'cairo', device = 'png')

normomo$new()$run_with_catch()

sykdomspuls_mem$new()$run_with_catch()

sykdomspuls_obs$new()$run_with_catch()

sykdomspuls_alert_pdf$new()$run_with_catch()

amort$new()$run_with_catch()

