sykdomspuls_date <- function(){
  fd::get_rundate()[package=="sykdomspuls"]$date_extraction
}
