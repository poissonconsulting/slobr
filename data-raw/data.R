conn <- DBI::dbConnect(RSQLite::SQLite(), "~/Poisson/Code/slobr/slobr/inst/extdata/demo_db.sqlite")

df <- data.frame(char = c("a", "b", "b"),
                 num = c(1.1, 2.2, 2.2),
                 key = c(1, 2, 3),
                 stringsAsFactors = FALSE)

df2 <- data.frame(char = c("a", "b", "b"),
                 int = c(1L, 2L, 3L),
                 stringsAsFactors = FALSE)

DBI::dbWriteTable(conn, "Table1", df2, overwrite = TRUE)
DBI::dbWriteTable(conn, "Table2", df, overwrite = TRUE)

write.csv(data.frame(x = 1), "~/Poisson/Code/slobr/slobr/inst/extdata/df.csv")
flob2 <- flobr::flob("~/Poisson/Code/slobr/slobr/inst/extdata/df.csv")

dbflobr::write_flob(flobr::flob_obj, "flob", "Table1", key = data.frame(int = 1L), 
                    conn = conn, exists = FALSE)

dbflobr::write_flob(flob2, "flob", "Table1", key = data.frame(int = 2L), 
                    conn = conn, exists = TRUE)

dbflobr::write_flob(flob2, "flob2", "Table1", key = data.frame(int = 1L), 
                    conn = conn, exists = FALSE)

DBI::dbDisconnect(conn)
