# path = system.file("extdata", "demo_db.sqlite", package = "slobr")
# conn <- pool_open(path)
# table_name = "Table2"

flob_datatable <- function(table_name, conn, mode, ns){
  table <- table_read(table_name, conn)
  blob_cols <- blob_column_names(table_name, conn)
  
  icon <- icon("download")
  if(mode == "write"){
    icon <- icon("upload")
  }
  print(icon)
  for(i in blob_cols){
    flobs <- table[[i]]
    types <- button_types(flobs, mode = mode)
    table[i] <- buttons(col = i, 
                        type = types, 
                        row = seq_len(nrow(table)),
                        ns = ns)
  }
  DT::datatable(table, escape = FALSE, selection = "none")
}

