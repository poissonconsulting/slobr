pool_open <- function(path){
  pool::dbPool(
    drv = RSQLite::SQLite(),
    dbname = path
  )
}

pool_close <- function(pool){
  onStop(function() {
    pool::poolClose(pool)
  })
}

table_names <- function(pool){
  DBI::dbListTables(pool)
}

table_read <- function(pool, table_name){
  DBI::dbReadTable(pool, name = table_name)
}
