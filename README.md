
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/poissonconsulting/slobr.svg?branch=master)](https://travis-ci.org/poissonconsulting/slobr)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/poissonconsulting/slobr?branch=master&svg=true)](https://ci.appveyor.com/project/poissonconsulting/slobr)
[![Codecov test
coverage](https://codecov.io/gh/poissonconsulting/slobr/branch/master/graph/badge.svg)](https://codecov.io/gh/poissonconsulting/slobr?branch=master)
<!-- badges: end -->

# slobr

`slobr` is a Shiny app to read and write files to SQLite databases as
[flobs](https://poissonconsulting.github.io/flobr/reference/flob.html).
A flob is a special type of BLOB that includes the file extension type.

## Installation

To install the latest development version from
[GitHub](https://github.com/poissonconsulting/slobr)

    remotes::install_github("poissonconsulting/shinyutils")
    remotes::install_github("poissonconsulting/flobr")
    remotes::install_github("poissonconsulting/dbflobr")
    remotes::install_github("poissonconsulting/slobr")

To install the latest development version from the Poisson
[drat](https://github.com/poissonconsulting/drat) repository.

    drat::addRepo("poissonconsulting")
    install.packages("slobr")

## Usage

``` r
library(slobr)
# to run with a demo database
slobr::run_app("demo")
```

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/slobr/issues).

[Pull requests](https://github.com/poissonconsulting/slobr/pulls) are
always welcome.

Please note that the ‘slobr’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.

## Creditation

  - [blob](https://github.com/tidyverse/blob)
  - [flobr](https://github.com/poissonconsulting/flobr)
  - [dbflobr](https://github.com/poissonconsulting/dbflobr)
