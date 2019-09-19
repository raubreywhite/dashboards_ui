fd::initialize("ui")
suppressMessages(library(data.table))
suppressMessages(library(ggplot2))

model <- normomo$new()
model$run_all()
