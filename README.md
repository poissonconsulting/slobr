
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

# slobr

`slobr` is a Shiny app to read and write files to SQLite databases as
[flobs](https://poissonconsulting.github.io/flobr/reference/flob.html).
A flob is a special type of BLOB that includes the file extension type.

## Installation

To install the latest development version from
[GitHub](https://github.com/poissonconsulting/slobr)

    # install.packages("devtools")
    devtools::install_github("poissonconsulting/checkr")
    devtools::install_github("poissonconsulting/shinyutils")
    devtools::install_github("poissonconsulting/flobr")
    devtools::install_github("poissonconsulting/dbflobr")
    devtools::install_github("poissonconsulting/slobr")

To install the latest development version from the Poisson
[drat](https://github.com/poissonconsulting/drat) repository.

    # install.packages("drat")
    drat::addRepo("poissonconsulting")
    install.packages("slobr")

## Usage

``` r
library(slobr)
# to run with a demo database
slobr::run_app("demo")
```

<!-- ## Citation -->

<!-- ```{r, comment="", echo=FALSE} -->

<!-- citation(package = "dbflobr") -->

<!-- ``` -->

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
