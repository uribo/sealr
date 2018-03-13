context("test-utils.R")

test_that("timestamp labeling", {
  skip_if_not_installed("sealr")
  expect_output(
    sealr_timestamp(),
    "by the sealr package"
  )
})
