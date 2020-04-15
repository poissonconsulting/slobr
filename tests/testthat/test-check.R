test_that("check_sqlite_connection", {
  expect_error(check_sqlite_connection(1), class = "chk_error")
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  expect_identical(check_sqlite_connection(conn), conn)
  expect_identical(check_sqlite_connection(conn, connected = TRUE), conn)
  expect_error(check_sqlite_connection(conn, connected = FALSE))
  DBI::dbDisconnect(conn)
  
  expect_identical(check_sqlite_connection(conn), conn)
  expect_error(check_sqlite_connection(conn, connected = TRUE))
  expect_identical(check_sqlite_connection(conn, connected = FALSE), conn)
})