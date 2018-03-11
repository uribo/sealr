context("test-object-context.R")

test_that("filter", {

  my_data1 <- iris
  my_data2 <- mtcars

  res <- filter_context("data.frame") %>%
    purrr::pmap(~ dim(..2))

  expect_is(
    res,
    "list")
  expect_length(
    res,
    2L)
  expect_equal(
    res[[1]],
    dim(iris))
  expect_equal(
    res[[2]],
    dim(mtcars))
})
