for(baseFolder in c("/data_clean","/results","/data_app","/config")){
  files <- list.files(file.path(baseFolder,"ui"))
  if(length(files)>0){
    for(f in files) unlink(file.path(baseFolder,"ui",f))
  }
}

unlink(file.path("/junit","ui.xml"))
Sys.sleep(1)

a <- testthat:::JunitReporter$new()
a$start_reporter()
a$out <- file(file.path("/junit","ui.xml"), "w+")
a$start_context("ui")

output <- processx::run("Rscript","/r/ui/src/RunProcess.R", error_on_status=F, echo=T)
cat("\n\nstdout\n\n")
cat(output$stdout)
cat("\n\nstderr\n\n")
cat(output$stderr)

if(output$status==0){
  a$add_result("ui","RunAll",testthat::expectation("success","Pass"))
} else {
  a$add_result("ui","RunAll",testthat::expectation("error","Fail"))
}

a$end_context("ui")
a$end_reporter()
close(a$out)



