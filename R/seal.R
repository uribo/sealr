#' Sealing the test results
#'
#' @description aaa
#'
#' @param test test
#' @param load_testthat include `library(testthat)` when *TRUE*
#' @param clip If *TRUE* will overwrite the system clipboard.
#'
#' @name seal
#' @examples
#' \dontrun{
#' tests <- rlang::expr_interp(paste('test_that("aa", {
#' expect_equal(', tests, ')})', sep = "\n"))
#' seal(tests, load_testthat = TRUE)
#' seal(tests, load_testthat = FALSE)
#' seal(tests, load_testthat = FALSE, clip = FALSE)
#' }
NULL

#' @rdname seal
#' @export
seal <- function(test, load_testthat = FALSE, clip = TRUE) {

  test_char <- test

  if (load_testthat == TRUE) {

    load_testthat_char <- "library(testthat)"

    test_char <- paste(load_testthat_char, test_char, sep = "\n")
  }

  res <- styler::style_text(
    test_char
  )

  if (clip == TRUE)
    res <- clipr::write_clip(res, "character", return_new = FALSE)

  return(res)
}
