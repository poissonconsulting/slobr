
<!-- README.md is generated from README.Rmd. Please edit that file -->

# slobr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/poissonconsulting/slobr.svg?branch=master)](https://travis-ci.org/poissonconsulting/slobr)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/poissonconsulting/slobr?branch=master&svg=true)](https://ci.appveyor.com/project/poissonconsulting/slobr)
[![Codecov test
coverage](https://codecov.io/gh/poissonconsulting/slobr/branch/master/graph/badge.svg)](https://codecov.io/gh/poissonconsulting/slobr?branch=master)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![CRAN
status](https://www.r-pkg.org/badges/version/slobr)](https://cran.r-project.org/package=slobr)
<!-- ![CRAN downloads](http://cranlogs.r-pkg.org/badges/slobr) -->

`slobr` is a Shiny app to read and write files to SQLite databases as
[flobs](https://poissonconsulting.github.io/flobr/reference/flob.html).
A flob is a special type of BLOB that includes the file extension type.

## Installation

To install the latest development version from
[GitHub](https://github.com/poissonconsulting/slobr)

    # install.packages("remotes")
    remotes::install_github("poissonconsulting/slobr")

To install the latest development version from the Poisson
[drat](https://github.com/poissonconsulting/drat) repository.

    drat::addRepo("poissonconsulting")
    install.packages("slobr")

## Demonstration

``` r
library(slobr)
# to run with a demo database
slobr::run_slobr("demo")

# to run with a local database, provide a SQLite Connection
dbname <- "/path/to/db.sqlite"
conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname)
slobr::run_slobr(conn)
```

## App Use

The SQLite Connection provided will be modified by the app. If you want
to test the functionality first, we suggest using the demo db provided
or create a copy of your own database.

Write files to a BLOB column within a table one cell/file at a time. To
create a new BLOB column see the ‘other options’ link for more buttons.

Read files from individual cells, entire columns or entire tables. These
will be provided to you in a nested zipped directory
(e.g. ‘table/column/file’).

Look for the question mark icon to get more instructions within the app.

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/slobr/issues).

[Pull requests](https://github.com/poissonconsulting/slobr/pulls) are
always welcome.

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/poissonconsulting/pkgtemplate/blob/master/CODE_OF_CONDUCT.md).
By contributing, you agree to abide by its terms.

## Creditation

  - [blob](https://github.com/tidyverse/blob)
  - [flobr](https://github.com/poissonconsulting/flobr)
  - [dbflobr](https://github.com/poissonconsulting/dbflobr)
