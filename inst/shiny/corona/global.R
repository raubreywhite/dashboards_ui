library(pool)

# load data in 'global' chunk so it can be shared by all users of the dashboard
pool <- dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = Sys.getenv("DB_DB", "sykdomspuls"),
  host = Sys.getenv("DB_SERVER", "db"),
  username =Sys.getenv("DB_USER", "root") ,
  password = Sys.getenv("DB_PASSWORD", "example")
)
config <- new.env()
config$ages <- list(
  "Totalt",
  "0-4",
  "5-14",
  "15-19",
  "20-29",
  "30-64",
  "65+"
)

config$start_date <- "2020-03-01"

