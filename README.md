
<!-- README.md is generated from README.Rmd. Please edit that file -->
sealr <img src="man/figures/logo.png" align="right" width="120px" />
====================================================================

[![CRAN status](https://www.r-pkg.org/badges/version/sealr)](https://cran.r-project.org/package=sealr) [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) [![Travis build status](https://travis-ci.org/uribo/sealr.svg?branch=master)](https://travis-ci.org/uribo/sealr) [![Coverage status](https://codecov.io/gh/uribo/sealr/branch/master/graph/badge.svg)](https://codecov.io/github/uribo/sealr?branch=master)

The goal of sealr is to ...

Installation
------------

**sealr** is not available on CRAN. So, install from GitHub.

``` r
install.packages("devtools")
devtools::install_github("uribo/sealr")
```

How to use
----------

``` r
library(sealr)
```

``` r
transcribe(iris) %>% 
  seal()
#> test_that("iris", {
#>   expect_is(
#>     iris,
#>     "data.frame"
#>   )
#>   expect_equal(
#>     dim(iris),
#>     c(150L, 5L)
#>   )
#>   expect_named(
#>     iris,
#>     c(
#>       "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
#>       "Species"
#>     )
#>   )
#>   expect_equal(
#>     iris %>% purrr::map_chr(class) %>% unname(),
#>     c("numeric", "numeric", "numeric", "numeric", "factor")
#>   )
#> })

transcribe(mtcars) %>% 
  seal()
#> test_that("mtcars", {
#>   expect_is(
#>     mtcars,
#>     "data.frame"
#>   )
#>   expect_equal(
#>     dim(mtcars),
#>     c(32L, 11L)
#>   )
#>   expect_named(
#>     mtcars,
#>     c(
#>       "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
#>       "gear", "carb"
#>     )
#>   )
#>   expect_equal(
#>     mtcars %>% purrr::map_chr(class) %>% unname(),
#>     c(
#>       "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
#>       "numeric", "numeric", "numeric", "numeric", "numeric"
#>     )
#>   )
#> })
```

-   `transcribe()`
-   `seal()`

| Object Type  | Class | Size | Name |
|--------------|-------|------|------|
| `vector`     | Y     | Y    | N    |
| `data.frame` | Y     | Y    | Y    |
| ...          | -     | -    | -    |

Code of Conduct
---------------

Please note that this project is released with a [Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md).

By participating in this project you agree to abide by its terms.
