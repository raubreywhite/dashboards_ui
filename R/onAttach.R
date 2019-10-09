.onAttach <- function(libname, pkgname) {
  packageStartupMessage("PACKAGE: ui")
  packageStartupMessage("Version 2019.10.08 at 06:54")
  packageStartupMessage(glue::glue("Developed by Richard White, Gunnar R{fhi::nb$oe}"))
  packageStartupMessage("Norwegian Institute of Public Health")
}
