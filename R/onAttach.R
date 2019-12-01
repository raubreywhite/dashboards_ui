.onAttach <- function(libname, pkgname) {
  packageStartupMessage("PACKAGE: ui")
  packageStartupMessage("Version 2019.12.01 at 21:20")
  packageStartupMessage(glue::glue("Developed by Richard White, Gunnar R{fhi::nb$oe}"))
  packageStartupMessage("Norwegian Institute of Public Health")
}
