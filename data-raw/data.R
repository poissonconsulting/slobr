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

instructions <- tagList(p(
  HTML(glue("Files appear in the table as file icons alongside the extension (e.g. {as.character(icon('file-pdf-o'))} pdf). 
          Any cell with no file appears as < ... >."))
), br(),
h5("Read/delete files"), 
p("Select one or more cells in the table and click 'read cell(s)' or 
'delete cell(s)' to read or delete just those cells.
          Click 'read column' or 'delete column' to read or delete all files
  in the columns containing the selected cells."),
br(),
h5("Write files"), 
p("To write a file, only one cell can be selected at at time. If a file already exists there it will be replaced."))

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
flob3 <- flobr::flob("~/Poisson/Code/slobr/slobr/inst/extdata/profile.jpg")
flob4 <- flobr::flob("~/Poisson/Code/slobr/slobr/inst/extdata/test.xlsx")

dbflobr::write_flob(flobr::flob_obj, "flob", "Table1", key = data.frame(int = 1L), 
                    conn = conn, exists = FALSE)

dbflobr::write_flob(flob2, "flob", "Table1", key = data.frame(int = 2L), 
                    conn = conn, exists = TRUE)

dbflobr::write_flob(flob2, "flob2", "Table1", key = data.frame(int = 1L), 
                    conn = conn, exists = FALSE)

dbflobr::write_flob(flob3, "flob2", "Table1", key = data.frame(int = 2L), 
                    conn = conn, exists = TRUE)

dbflobr::write_flob(flob4, "flob2", "Table1", key = data.frame(int = 3L), 
                    conn = conn, exists = TRUE)

DBI::dbDisconnect(conn)
