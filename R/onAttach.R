.onAttach <- function(libname, pkgname) {
  packageStartupMessage("PACKAGE: ui")
  packageStartupMessage("Version 2019.09.30 at 11:03")
  packageStartupMessage(glue::glue("Developed by Richard White, Gunnar R{fhi::nb$oe}"))
  packageStartupMessage("Norwegian Institute of Public Health")
}
