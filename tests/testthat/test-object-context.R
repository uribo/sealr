context("test-object-context.R")

test_that("collect object", {
  expect_gte(nrow(global_objects()),
             1L)
  expect_is(nms_objects(),
            "data.frame")
  expect_equal(dim(nms_objects(pkgs = "package:datasets")),
               c(104, 3))
  expect_gte(nrow(nms_objects(
    pkgs = c("package:datasets", "package:utils")
  )),
  310L)
})

test_that("filter", {
  expect_equal(ncol(
    ls_objects(
      class = "data.frame",
      pkgs = "package:datasets",
      nms = TRUE,
      eval = TRUE
    )
  ),
  4L)

  skip_on_os("mac")
  expect_message(
    ls_objects(
      class = "function",
      pkgs = "package:ggplot2",
      nms = TRUE,
      eval = FALSE
    ),
    "The given environment is not stored any objects."
  )

  e <- rlang::env()
  withr::with_environment(e, {
    suppressMessages(library(dplyr))
    expect_is(
      ls_objects(
        class = "function",
        pkgs = "package:dplyr",
        nms = TRUE,
        eval = FALSE
      ),
      "tbl"
    )
  })
})
