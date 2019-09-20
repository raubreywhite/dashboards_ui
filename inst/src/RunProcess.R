fd::initialize("ui")
suppressMessages(library(data.table))
suppressMessages(library(ggplot2))

model <- normomo$new()
model$run_all()

model <- sykdomspuls_alert_pdf$new()
model$run_all()

model <- sykdomspuls_email_obs$new()
model$run_all()

