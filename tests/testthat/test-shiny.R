context("shiny tests")

test_that("app ui", {
  ui <- app_ui()
  expect_is(ui, "shiny.tag.list")
})

test_that("app server", {
  server <- app_server
  expect_is(server, "function")
})

test_that("shiny functions work", {
  conn <- db_connect(system.file("extdata", "demo_db.sqlite", package = "slobr")) 
  table <- table_read("Table1", conn)
  x <- flob_datatable(table, "Table1", conn, ns = shiny::NS)
  expect_is(x, c("datatables", "htmlwidget"))
  
  x <- cell_display("pdf")
  names(x) <- NULL
  expect_identical(x, "<i class=\"fa fa-file-pdf-o\"></i> pdf")
  expect_identical(ext_icon("pdf"), "file-pdf-o")
  
  x <- info_tooltip(label_container("a"), "yup")
  expect_is(x, "shiny.tag")
  
  mat <- matrix(c(3, 2), ncol = 2, byrow = FALSE)

  x <- write_modal(mat, "Table1", conn, shiny::NS("yup"))
  expect_is(x, "shiny.tag")
  x <- add_column_modal(shiny::NS("yup"))
  expect_is(x, "shiny.tag.list")
  x <- delete_modal(mat, "Table1", conn, "cell")
  expect_true(x)
  x <- read_modal(mat, "Table1", conn, "cell")
  expect_true(x)
})





