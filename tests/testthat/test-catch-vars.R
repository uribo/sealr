context("test-catch-vars.R")

test_that("catch up data.frame variable names and value", {
  expect_identical(catch_varnames(iris),
                   paste0(
                     "iris$",
                     names(iris)
                   ))
  expect_equal(catch_varname(iris, variable = "Sepal.Length"),
               "iris$Sepal.Length")
  expect_error(catch_varname(iris, c("Sepal.Length", "Species")),
               "Enable a single variable name")
  expect_error(catch_varnames(letters),
              "Only applied to an object of class \"data.frame\"")

  skip_if_not_installed("glue")
  expect_equal(glue::evaluate(catch_varname(mtcars, "mpg"), .GlobalEnv),
               mtcars$mpg)
})
