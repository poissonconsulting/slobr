library(shiny)
library(glue)
library(readwritesqlite)

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

usethis::use_data(file_types, instructions, internal = TRUE, overwrite = TRUE)

unlink("~/Code/slobr/slobr/inst/extdata/demo_db.sqlite")
conn <- readwritesqlite::rws_connect("~/Code/slobr/slobr/inst/extdata/demo_db.sqlite", exists = FALSE)

df <- data.frame(char = c("a", "b", "b"),
                 num = c(1.1, 2.2, 2.2),
                 key = c(1, 2, 3),
                 null = NA_character_,
                 stringsAsFactors = FALSE)

df2 <- data.frame(char = c("a", "b", "c"),
                 int = c(1L, 2L, 2L),
                 stringsAsFactors = FALSE)

readwritesqlite::rws_write(df2, x_name = "Table1", conn = conn, exists = FALSE, replace = TRUE)
readwritesqlite::rws_write(df, x_name = "Table2", conn = conn, exists = FALSE, replace = TRUE)
readwritesqlite::rws_write(readwritesqlite::rws_data, x_name = "RwsData", conn = conn, exists = FALSE, replace = TRUE)

write.csv(data.frame(x = 1), "~/Code/slobr/slobr/inst/extdata/df.csv")
flob2 <- flobr::flob("~/Code/slobr/slobr/inst/extdata/df.csv")
flob3 <- flobr::flob("~/Code/slobr/slobr/inst/extdata/file.jpg", name = "profile")
flob4 <- flobr::flob("~/Code/slobr/slobr/inst/extdata/test.xlsx")

dbflobr::write_flob(flobr::flob_obj, "flob", "Table1", 
                    key = data.frame(int = 2L, char = "c", 
                                     stringsAsFactors = FALSE), 
                    conn = conn, exists = FALSE)

dbflobr::write_flob(flob2, "flob", "Table1", 
                    key = data.frame(int = 2L, char = "b", 
                                     stringsAsFactors = FALSE), 
                    conn = conn, exists = TRUE)

dbflobr::write_flob(flob2, "flob2", "Table1", key = data.frame(int = 1L), 
                    conn = conn, exists = FALSE)

dbflobr::write_flob(flob3, "flob2", "Table1", 
                    key = data.frame(int = 2L, char = "c", 
                                     stringsAsFactors = FALSE), 
                    conn = conn, exists = TRUE)

DBI::dbDisconnect(conn)
