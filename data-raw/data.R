file_types <- list(
  xl = list(ext = c("xls", "xlsx"), icon = "file-excel"),
  ms = list(ext = c("doc", "docx"), icon = "file-word"),
  snd = list(ext = c("mp3", "mp4", "wav", "m4a"), icon = "file-sound"),
  txt = list(ext = c("txt"), icon = "file-text"),
  pdf = list(ext = c("pdf"), icon = "file-pdf"),
  img = list(ext = c("png", "jpg"), icon = "file-picture"),
  csv = list(ext = c("csv"), icon = "file"),
  mov = list(ext = c("mov"), icon = "file-movie"),
  ppt = list(ext = c("ppt"), icon = "file-powerpoint"),
  zip = list(ext = c("zip"), icon = "file-zip"),
  code = list(ext = c("r", "rds", "js"), icon = "file-code")
)

instructions <- "Files appear in the table as file icons alongside the extension. 
          Any cell in a BLOB column without a file appears as < ... >. 
                       Files may be written to empty cells
                       or may replace existing files. 
                       You may read and delete files from multiple cells at once. 
                       You may write files to only one cell at a time."

usethis::use_data(file_types, instructions, internal = TRUE, overwrite = TRUE)

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
