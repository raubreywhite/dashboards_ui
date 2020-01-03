.onAttach <- function(libname, pkgname) {
  packageStartupMessage("PACKAGE: ui")
  packageStartupMessage("Version 2020.01.03 at 08:09")
  packageStartupMessage(glue::glue("Developed by Richard White, Gunnar R{fhi::nb$oe}"))
  packageStartupMessage("Norwegian Institute of Public Health")
}
