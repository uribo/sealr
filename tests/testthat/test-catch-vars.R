context("test-catch-vars.R")

test_that("catch up data.frame variable names and value", {
  expect_is(store_to_env(iris), "environment")
  expect_identical(catch_varnames(iris),
                   paste0(
                     "iris$",
                     c(
                       "Petal.Length",
                       "Sepal.Width",
                       "Petal.Width",
                       "Sepal.Length",
                       "Species"
                     )
                   ))
  expect_equal(catch_varname(iris, variable = "Sepal.Length"),
               "iris$Sepal.Length")
  expect_error(catch_varname(iris, c("Sepal.Length", "Species")),
               "Enable a single variable name")

  skip_if_not_installed("glue")
  expect_equal(glue::evaluate(catch_varname(mtcars, "mpg"), .GlobalEnv),
               mtcars$mpg)
})

test_that("Return valiable names match but different order", {
  expect_true(identical(
    catch_varnames(iris) %>% sort(),
    paste0("iris$", names(iris)) %>% sort()
  ))
  expect_false(identical(catch_varnames(iris),
                         names(iris)))
})
