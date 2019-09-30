fd::initialize("ui")
suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
options(bitmapType = 'cairo', device = 'png')

sykdomspuls_mem$new()$run_all()

## normomo$new()$run_all()

## sykdomspuls_alert_pdf$new()$run_all()

## sykdomspuls_obs$new()$run_all()


