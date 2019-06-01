conn <- DBI::dbConnect(RSQLite::SQLite(), "./inst/extdata/demo_db.sqlite")

df <- data.frame(char = c("a", "b", "b"),
                 num = c(1.1, 2.2, 2.2),
                 key = c(1, 2, 3),
                 stringsAsFactors = FALSE)

df2 <- data.frame(char = c("a", "b", "b"),
                 int = c(1L, 2L, 3L),
                 stringsAsFactors = FALSE)

DBI::dbWriteTable(conn, "Table1", df)
DBI::dbWriteTable(conn, "Table2", df2)

dbflobr::write_flob(flobr::flob_obj, "flob", "Table2", key = data.frame(int = 1L), 
                    conn = conn, exists = FALSE)

DBI::dbDisconnect(conn)
