context("test-utils.R")

test_that("multiplication works", {
  skip_if_not_installed("sealr")
  expect_output(
    sealr_timestamp(),
    paste0("ℹ: Created on ",
           Sys.Date(),
           " by the sealr package \\(v",
           as.character(packageVersion("sealr")),
           ")")
  )
})
