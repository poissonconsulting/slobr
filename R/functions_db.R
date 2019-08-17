db_connect <- function(path){
  if(is.null(path)) return(NULL)
  readwritesqlite::rws_connect(path)
}

table_names <- function(conn){
  if(is.null(conn)) return(NULL)
  readwritesqlite::rws_list_tables(conn)
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

sfc_columns <- function(table_name, conn){
  has_meta <- "readwritesqlite_meta" %in% table_names(conn)
  if(isTRUE(has_meta)){
    return({
      meta <- table_read("readwritesqlite_meta", conn)
      meta <- meta[meta$TableMeta == toupper(table_name),]
      meta$ColumnMeta[grepl("^proj:", meta$MetaMeta)]
    })
  }
  NULL
}

blob_columns <- function(table_name, conn){
  cols <- column_names(table_name, conn)
  sfc_cols <- sfc_columns(table_name, conn)
  which(sapply(cols, USE.NAMES = FALSE, function(x){
    is_column_blob(x, table_name, conn) && !(toupper(x) %in% sfc_cols)
  }))
}

column_exists <- function(x, table_name, conn){
  column_names <- column_names(table_name, conn)
  toupper(x) %in% toupper(column_names)
}
