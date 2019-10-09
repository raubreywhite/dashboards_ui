#' Mem -outputs
#' @import R6
#' @export
UIBase <- R6::R6Class(
  "UIBase",
  portable = FALSE,
  cloneable = TRUE,
  list(
    run_with_catch = function() {
      print(class(self)[1])
      tryCatch({
        self$run_all()
      },
      error = function(e) {
        if (fd::config$is_production) {
          fd::msg("ERROR", slack = T)
          fd::msg(e, slack = T)
        } else {
          stop(e)
        }
      }
      )
    },
    run_all = function() {
      stop("run_all must be implemented")
    }
  )
)
