context("test-utils.R")

test_that("timestamp labeling", {
  skip_if_not_installed("sealr")
  expect_output(
    sealr_timestamp(),
    "by the sealr package"
  )
})

test_that("check lhs object statement", {
  x <- c(1, 3)
  e <- new.env()
  assign("x", x, e)
  withr::with_environment(e, {
    expect_s3_class(x %>%
                      lhs_obj(),
                    "tbl_df")
    expect_equal(nrow(x %>%
                        lhs_obj()),
                 1L)
    expect_equal(dim(x %>%
                       lhs_obj()),
                 c(1, 4))
    expect_named(x %>%
                   lhs_obj(),
                 c("environment", "name", "class", "hash"))
    expect_equal(x %>%
                   lhs_obj() %>% .$class,
                 "numeric")

    expect_equal(x %>%
                   lhs_name(),
                 "x")
  })

})

test_that("Failure un-saved values", {
  expect_equal(expect_message(
    nrow(c(1, 3) %>%
           lhs_obj()),
    "The given object is not stored in any environment."
  ),
  0L)

  expect_message(data.frame(x = letters) %>%
                   subset(x == "a") %>%
                   lhs_obj())
})
