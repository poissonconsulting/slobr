# path = system.file("extdata", "demo_db.sqlite", package = "slobr")
# conn <- pool_open(path)
# table_name = "Table2"

flob_datatable <- function(table_name, conn, ns){
  table <- table_read(table_name, conn)
  blob_cols <- blob_column_names(table_name, conn)
  for(i in blob_cols){
    table[i] <- download_buttons(nrow(table), id = glue("_{i}_"), 
                        label = "Download", ns = ns)
  }
  DT::datatable(table, escape = FALSE, selection = "none")
}

