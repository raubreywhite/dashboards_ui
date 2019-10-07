fd::initialize("ui")
suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
options(bitmapType = 'cairo', device = 'png')

#weather_download$new()$run_all()

sykdomspuls_mem$new()$run_with_catch()


normomo$new()$run_all()



sykdomspuls_obs$new()$run_all()



amort$new()$run_all()


