.quotes <- "^(`|[[]|\")(.*)(`|[]]|\")$"

is_quoted <- function(x) grepl(.quotes, x)

to_upper <- function(x) {
  x <- as.character(x)
  is_quoted <- is_quoted(x)
  x[!is_quoted] <- toupper(x[!is_quoted])
  x
}

is_err <- function(x) inherits(x, "try-error")

rm_null <- function(x) x[lengths(x) != 0]

br2 <- function() tagList(br(), br())