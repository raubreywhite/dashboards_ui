fd::initialize("ui")
suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
options(bitmapType = 'cairo', device = 'png')

print("**A")
normomo$new()$run_with_catch()
print("**B")
normomo$new()$run_with_catch()
print("**C")
#sykdomspuls_mem$new()$run_with_catch()

sykdomspuls_obs$new()$run_with_catch()

sykdomspuls_alert_pdf$new()$run_with_catch()

sykdomspulspdf$new()$run_with_catch()

#amort$new()$run_with_catch()

