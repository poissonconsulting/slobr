test_that("data functions work", {
  flob <- flobr::flob_obj
  conn <- db_connect(system.file("extdata", "demo_db.sqlite", package = "slobr")) 
  
  expect_identical(flob_ext(flob), "pdf")
  expect_identical(flob_ext("nope"), "empty")
  expect_identical(flob_exts(c(flob, flob)), c("pdf", "pdf"))
  
  mat <- matrix(c(1, 2), ncol = 2, byrow = FALSE)
  mat2 <- matrix(c(1:2, rep(2, 2)), ncol = 2, byrow = FALSE)
  
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
  expect_equal(flobs[[1]], flob)
  
  x <- paste(file_name(mat, "Table1", conn, by = "cell"))
  expect_identical("flobr.pdf", x)
  x <- paste(file_name(mat, "Table1", conn, by = "column"))
  expect_identical("Table1.zip", x)
  x <- paste(file_name(mat2, "Table1", conn, by = "cell"))
  expect_identical("Table1.zip", x)
  
})

