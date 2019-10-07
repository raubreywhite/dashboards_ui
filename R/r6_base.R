#' Mem -outputs
#' @import R6
#' @export sykdomspuls_mem
UIBase<- R6::R6Class(
  "UIBase",
  portable = FALSE,
  cloneable = TRUE,
  list(
    run_with_catch = function() {
      tryCatch({
        self$run_all()
      },
      error=function(e){
        if(fd::config$is_production){
          fd::msg("ERROR", slack=T)
        } else {
          stop("ERROR")
        }
      }
      )
    }
   ,run_all = function(){
      stop("run_internal must be implemented")
      
    }
  )
)
