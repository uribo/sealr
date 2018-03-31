
<!-- README.md is generated from README.Rmd. Please edit that file -->
sealr <img src="man/figures/logo.png" align="right" width="120px" />
====================================================================

[![CRAN status](https://www.r-pkg.org/badges/version/sealr)](https://cran.r-project.org/package=sealr) [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) [![Travis build status](https://travis-ci.org/uribo/sealr.svg?branch=master)](https://travis-ci.org/uribo/sealr) [![Coverage status](https://codecov.io/gh/uribo/sealr/branch/master/graph/badge.svg)](https://codecov.io/github/uribo/sealr?branch=master)

The goal of sealr is to reduce the burden of writing unit tests and assertion that record the state of objects. Applying a function of sealr (`design_*()` or `transcribe()`) to the target object outputs the test code that record the current state.

How to use
----------

``` r
library(sealr)
```

``` r
x <- seq(1, 9, by = 2)

design_class(x, seal = TRUE)
#> Warning: clipr not available. check clipr configuration.
#> #' ℹ: Labeling on 2018-03-31 by the sealr package (v0.1.0)
#> expect_is(
#>   x,
#>   "numeric"
#> )

design_range(x, seal = TRUE)
#> Warning: clipr not available. check clipr configuration.
#> #' ℹ: Labeling on 2018-03-31 by the sealr package (v0.1.0)
#> expect_equal(
#>   range(x, na.rm = TRUE),
#>   c(1, 9)
#> )
```

``` r
transcribe(iris)
#> #' ℹ: Labeling on 2018-03-31 by the sealr package (v0.1.0)
#> test_that("check iris statement", {
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
#>     iris %>% purrr::map(class) %>% unname(),
#>     list("numeric", "numeric", "numeric", "numeric", "factor")
#>   )
#> })

transcribe(mtcars, load_testthat = FALSE, ts = FALSE)
#> test_that("check mtcars statement", {
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
#>     mtcars %>% purrr::map(class) %>% unname(),
#>     list(
#>       "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
#>       "numeric", "numeric", "numeric", "numeric", "numeric"
#>     )
#>   )
#> })
```

### APIs

-   `design_*()`
-   `transcribe()`

Code of Conduct
---------------

Please note that this project is released with a [Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md).

By participating in this project you agree to abide by its terms.
