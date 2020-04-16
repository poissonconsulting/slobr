test_that("data functions work", {
  flob <- flobr::flob_obj
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  teardown(DBI::dbDisconnect(conn))
  
  df2 <- data.frame(char = c("a", "b", "c"),
                    int = c(1L, 2L, 2L),
                    stringsAsFactors = FALSE)
  
  DBI::dbWriteTable(conn, "Table1", df2)

  flob2 <- flobr::flob(system.file("extdata/df.csv", package = "slobr"))
  flob3 <- flobr::flob(system.file("extdata/file.jpg", package = "slobr"), name = "profile")
  flob4 <- flobr::flob(system.file("extdata/test.xlsx", package = "slobr"))
  
  dbflobr::write_flob(flob, "flob", "Table1", 
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
  
  expect_identical(flob_ext(flob), "pdf")
  expect_identical(flob_ext("nope"), "empty")
  expect_identical(flob_exts(c(flob, flob)), c("pdf", "pdf"))
  
  mat <- matrix(c(3, 2), ncol = 2, byrow = FALSE)
  mat2 <- matrix(c(2:3, rep(2, 2)), ncol = 2, byrow = FALSE)
  
  x <- column_matrix(mat2, "Table1", conn)
  expect_is(x, "matrix")
  expect_true(nrow(x) == 3)
  expect_identical(unique(mat2[,2]), unique(x[,2]))
  
  x <- table_matrix("Table1", conn)
  expect_is(x, "matrix")
  expect_true(nrow(x) == 6)
  expect_identical(unique(x[,2]), c(2, 3))
  
  expect_identical(x, get_matrix(mat, "Table1", conn, by = "table"))
  
  key <- key_matrix(mat2, "Table1", conn)
  expect_is(key, "list")
  expect_equal(length(key), 2)
  expect_identical(names(key[[1]]), c("column_name", "key"))
  
  flobs <- get_flobs(mat2, "Table1", conn)
  expect_is(flobs, "list")
  expect_equal(length(key), 2)
  
  expect_is(flobs[[1]], c("flob", "blob"))
  names(flob) <- "flob"
  expect_identical(flobs[[1]][[1]], flob2[[1]])
  
  x <- paste(file_name(mat, "Table1", conn, by = "cell"))
  expect_identical("flobr.pdf", x)
  x <- paste(file_name(mat, "Table1", conn, by = "column"))
  expect_identical("Table1.zip", x)
  x <- paste(file_name(mat2, "Table1", conn, by = "cell"))
  expect_identical("Table1.zip", x)
  
  cells <- matrix(data = c(2, 3), nrow = 1, ncol = 2)
  x <- send_flob(cells, table_name = "Table1", conn = conn, system.file("extdata/df.csv", package = "slobr"), name = "df")
  expect_is(x, "flob")
  cells2 <- matrix(data = c(2, 4), nrow = 1, ncol = 2)
  x <- send_flob(cells2, table_name = "Table1", conn = conn, system.file("extdata/df.csv", package = "slobr"), name = "df")
  expect_is(x, "try-error")
  
  y <- dbflobr::read_flob("flob2", "Table1", key[[1]]$key, conn)
  expect_is(y, "flob")
  
  cells <- matrix(data = c(2, 2), nrow = 1, ncol = 2)
  x <- delete_flob(cells, "Table1", conn, by = "cell")
  expect_is(x[[1]], "flob")
  expect_identical(sum(is.na(table_read("Table1", conn)$flob)), 2L)
  
  cells <- matrix(data = c(2, 3), nrow = 1, ncol = 2)
  expect_identical(sum(!is.na(table_read("Table1", conn)$flob2)), 3L)
  x <- delete_flob(cells, "Table1", conn, by = "column")
  expect_true(all(is.na(table_read("Table1", conn)$flob2)))
  
  x <- add_column("new", table_name = "Table1", conn)
  expect_true(x)
  expect_true("new" %in% names(table_read("Table1", conn)))
  
})

