.quotes <- "^(`|[[]|\")(.*)(`|[]]|\")$"

is_quoted <- function(x) grepl(.quotes, x)

to_upper <- function(x) {
  x <- as.character(x)
  is_quoted <- is_quoted(x)
  x[!is_quoted] <- toupper(x[!is_quoted])
  x
}

title <- function(x) h3(x, style = "text-align: left; border-bottom: 1px solid #494949; font-size: 16px; margin: 10px;")

is_try_error <- function(x) inherits(x, "try-error")

rm_null <- function(x) x[lengths(x) != 0]

rm_ext <- function(x) sub(pattern = "(.*?)\\..*$", replacement = "\\1", x)

br2 <- function() tagList(br(), br())

button <- function(id, label = "Get Data (csv)", icon = "download", status = "primary"){
  tags$button(id = id,
              type = "button",
              class = glue::glue("btn action-button btn-md btn-{status}"),
              HTML(as.character(icon(icon)), label))
}

click_js <- function(id){
  glue::glue("document.getElementById('{id}').click();")
}