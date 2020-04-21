.onAttach <- function(libname, pkgname) {
  packageStartupMessage("PACKAGE: ui")
  packageStartupMessage("Version 2020.04.21 at 14:53")
  packageStartupMessage(glue::glue("Developed by Richard White, Gunnar R{fhi::nb$oe}"))
  packageStartupMessage("Norwegian Institute of Public Health")
}
