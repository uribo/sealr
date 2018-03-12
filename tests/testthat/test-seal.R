context("test-seal.R")

test_that("sealing", {
  tests <- transcribe(3.14)

  expect_silent(seal(tests, load_testthat = TRUE, clip = FALSE))

  expect_warning(
    seal(tests, clip = TRUE),
    "clipr not available. check clipr configuration."
  )

})

test_that("clip", {
  tests <- transcribe(3.14)
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_on_os("linux")
  skip_on_os("solaris")
  expect_silent(seal(tests, clip = TRUE))
})
