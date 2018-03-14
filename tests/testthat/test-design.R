context("test-design.R")

test_that("control transcript behavior functions", {

  expect_equal(
    design_expect_nrow(iris),
    "expect_equal(\nnrow(iris),\n150L)"
  )
  expect_equal(
    design_expect_ncol(mtcars),
    "expect_equal(\nncol(mtcars),\n11L)"
  )

  # Fixed #2
  expect_length(
    design_expect_length(x = c(letters, LETTERS)),
    1L
  )

  expect_equal(
    design_expect_unique_length(c("a", "a", "b")),
    "expect_length(\nunique(c(\"a\", \"a\", \"b\")),\n2L)"
  )

  expect_equal(
    desing_expect_missings(x = c(1, NA, 3)),
    "expect_equal(\nsum(is.na(c(1, NA, 3))),\n1L)"
  )

  iris_s4 <- asS4(iris)
  expect_equal(
    design_expect_class(iris_s4),
    "expect_s4_class(\niris_s4,\n\"data.frame\")"
    )
})
