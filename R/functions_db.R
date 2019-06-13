pool_open <- function(path){
  if(is.null(path)) return(NULL)
  pool::dbPool(
    drv = RSQLite::SQLite(),
    dbname = path
  )
}

pool_close <- function(conn){
  onStop(function() {
    pool::poolClose(conn)
  })
}

table_names <- function(conn){
  if(is.null(conn)) return(NULL)
  DBI::dbListTables(conn)
}

column_names <- function(table_name, conn) {
  DBI::dbListFields(conn, table_name)
}

table_read <- function(table_name, conn){
  if(is.null(conn)) return(NULL)
  DBI::dbReadTable(conn, name = table_name)
}

get_query <- function(sql, conn) {
  DBI::dbGetQuery(conn, sql)
}

execute <- function(sql, conn) {
  DBI::dbExecute(conn, sql)
}

sql_interpolate <- function(sql, conn, ...) {
  DBI::sqlInterpolate(conn, sql, ...)
}

table_info <- function(table_name, conn) {
  sql <- glue("PRAGMA table_info('{table_name}');")
  table_info <- get_query(sql, conn)
  table_info
}

table_column_type <- function(column_name, table_name, conn) {
  table_info <- table_info(table_name, conn)
  table_info$type[to_upper(table_info$name) == to_upper(column_name)]
}

is_column_blob <- function(column_name, table_name, conn) {
  toupper(table_column_type(column_name, table_name, conn)) == "BLOB"
}

blob_column_names <- function(table_name, conn){
  cols <- column_names(table_name, conn)
  cols[which(sapply(cols, USE.NAMES = FALSE, function(x){
    is_column_blob(x, table_name, conn)
  }))]
}

column_exists <- function(x, table_name, conn){
  column_names <- column_names(table_name, conn)
  x %in% column_names
}
