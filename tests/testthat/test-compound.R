context("test-compound.R")

test_that("Storage a single object in temporary environment", {
  expect_is(compound(letters), "environment")
  expect_is(iris %>% compound(), "environment")
})

test_that("Can evaluation object", {
  e <- compound(letters)
  expect_length(ls(e, all.names = TRUE),
                1L)

  e <- iris %>%
    compound()
  expect_equal(match(".obj", ls(e, all.names = TRUE)),
               1L)
  expect_identical(e$`iris$Petal.Length`,
                   iris$Petal.Length)
})
