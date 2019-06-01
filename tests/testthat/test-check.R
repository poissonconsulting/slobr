test_that("check_sqlite_connection", {
  expect_error(check_sqlite_connection(1),
               "1 must inherit from class SQLiteConnection")
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  expect_identical(check_sqlite_connection(conn), conn)
  expect_identical(check_sqlite_connection(conn, connected = TRUE), conn)
  expect_error(check_sqlite_connection(conn, connected = FALSE),
               "conn must be disconnected")
  DBI::dbDisconnect(conn)
  
  expect_identical(check_sqlite_connection(conn), conn)
  expect_error(check_sqlite_connection(conn, connected = TRUE),
               "conn must be connected")
  expect_identical(check_sqlite_connection(conn, connected = FALSE), conn)
})