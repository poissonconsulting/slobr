data_table <- function(pool, table_name){
  table <- table_read(pool, table_name)
  DT::datatable(table)
}