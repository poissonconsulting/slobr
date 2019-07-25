test_that("css functions work", {
  expect_is(css_add("test"), "shiny.tag")
  expect_is(css_navbar(), "shiny.tag")
  expect_is(css_hide_errors(), "shiny.tag")
})

