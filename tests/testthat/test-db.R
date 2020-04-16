test_that("db functions work", {
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  teardown(DBI::dbDisconnect(conn))
  
  df <- data.frame(char = c("a", "b", "b"),
                   num = c(1.1, 2.2, 2.2),
                   key = c(1, 2, 3),
                   null = NA_character_,
                   stringsAsFactors = FALSE)
  
  df2 <- data.frame(char = c("a", "b", "c"),
                    int = c(1L, 2L, 2L),
                    stringsAsFactors = FALSE)
  
  DBI::dbWriteTable(conn, "Table1", df2)
  DBI::dbWriteTable(conn, "Table2", df)

  flob2 <- flobr::flob(system.file("extdata/df.csv", package = "slobr"))
  flob3 <- flobr::flob(system.file("extdata/file.jpg", package = "slobr"), name = "profile")
  flob4 <- flobr::flob(system.file("extdata/test.xlsx", package = "slobr"))
  
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
  
  x <- table_names_all(conn)
  expect_identical(x, c("Table1", "Table2"))
  expect_identical(table_names(conn), c("Table1", "Table2"))
  
  x <- column_names("Table1", conn)
  expect_identical(x, c("char", "int", "flob", "flob2"))
  
  df <- table_read("Table1", conn)
  expect_is(df, "data.frame")
  expect_identical(x, colnames(df))
  
  y <- table_info("Table1", conn)
  expect_is(y, "data.frame")
  expect_identical(y$name, colnames(df))
  
  y <- table_column_type("flob", "Table1", conn)
  expect_identical(y, "BLOB")
  expect_true(is_column_blob("flob", "Table1", conn))
  expect_length(sfc_columns("Table1", conn), 0)
  expect_identical(blob_columns("Table1", conn), c(3L, 4L))
  
  # check sfc column
  conn <- DBI::dbConnect(RSQLite::SQLite(), system.file("inst/extdata/demo_db.sqlite", package = "slobr"))
  expect_identical(sfc_columns("RwsData", conn), "GEOMETRY")
  
})

