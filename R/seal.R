#' Sealing the test results
#'
#' @description aaa
#'
#' @param test test
#' @param load_testthat include `library(testthat)` when *TRUE*
#' @param clip If *TRUE* will overwrite the system clipboard.
#' When clipr is not available, The clip arguments is forcibly *FALSE*.
#'
#' @name seal
#' @examples
#' \dontrun{
#' tests <- transcribe(3.14)
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
    if (clipr::clipr_available() == FALSE) {
      rlang::warn("clipr not available. check clipr configuration.")
    } else {
      res <- clipr::write_clip(res, "character", return_new = FALSE)
    }


  return(res)
}
