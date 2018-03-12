context("test-object-context.R")

test_that("collect object",{
  expect_equal(
    nrow(collect_objects(ws = NULL)),
    0L
  )
})

test_that("filter", {

  e <- new.env()
  e$my_data1 <- iris
  e$my_data2 <- mtcars

  withr::with_environment(
    e, {
      res <- filter_context(context = "data.frame", ws = e) %>%
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
    }
  )

})
