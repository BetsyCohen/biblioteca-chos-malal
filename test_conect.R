# install.packages(c(
#   "DBI",
#   "RPostgres"
# ))

# test conexion a dbi
library(DBI)
library(RPostgres)

con <- dbConnect(
  Postgres(),
  host = "127.0.0.1",
  port = 5433,
  dbname = "biblioteca",
  user = "admin",
  password = "admin123"
)

dbGetQuery(con, "SELECT current_user;")

dbListTables(con)
