test_that("db functions work", {
  conn <- db_connect(system.file("extdata", "demo_db.sqlite", package = "slobr"))
  expect_is(conn, "SQLiteConnection")
  
  x <- table_names_all(conn)
  expect_identical(x, c("RwsData", "Table1", "Table2",
                        "readwritesqlite_init",
                        "readwritesqlite_log",
                        "readwritesqlite_meta"))
  expect_identical(table_names(conn), c("RwsData", "Table1", "Table2"))
  
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
  expect_identical(sfc_columns("RwsData", conn), "GEOMETRY")
  
})

