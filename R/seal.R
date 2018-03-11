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




# seal_df <- function(object) {
#   if (is.data.frame(object)) {
#     res <- structure(
#       dimension = dim(object)
#     )
#     return(res)
#   }
#
#   NULL
#
# }
#
# seal_df(iris)
#
#
# writeLines(
#   expression(
#     test_that("aaa",{
#       expect_equal(
#         dim(iris),
#         rlang::expr_text(dim(iris))
#       )
#     })
#   ) %>% as.character(),
#   "hoge.R"
# )
#
# ## substitute(dim(iris)) %>% as.character()
# rlang::parse_expr()
#
#
# (function() {
#   test_that("aa", {
#     expect_equal(
#       dim(iris), c(150L, 5L)
#     )
#   })
# })
#
#
# xx <- styler::style_text('library(testthat)\n
# test_that("aa", {
# expect_equal(dim(iris), c(150, 5))
# })\n
# mtcars')
#
# writeLines(
#   xx,
#   "inst/hoge.R"
# )
#
#
#
#
# cat(
# 'library(testthat)\n
# test_that("aa", {
# expect_equal(dim(iris), c(150, 5))
# })\n
# mtcars',
#   file = "inst/hoge.R")
# x <- parse_exprs(file("inst/hoge.R"))
# eval(x[[1]])
# eval(x[[2]])
# x
#
# parse_quos()
#
# rlang::expr_print(dim(iris))
# rlang::expr_label(dim(iris))
# rlang::expr_deparse(dim(iris))
# rlang::expr_print(dim(iris))
# rlang::expr_interp(dim(iris))
#
# rlang::expr(dim(iris))

# seal <- function() {
#   structure(list(
#     # 名前をつけないと出力されない
#     aaa = "aaa"), class = "seal")
# }
# seal()
#
# as.character.seal <- function(x, ...) {
#   paste(c(cli::rule("Seal"),
#           x$aaa),
#         "\n")
# }
#
# as.character.seal(seal())
#
# print.seal <- function(x, ...) {
#   cat(as.character(x))
# }
# print.seal(seal())
#
#
# seal()
# methods("print") %>%
#   grep("seal", ., value = TRUE)


# generic methods ---------------------------------------------------------------------
# ref) https://github.com/tidyverse/broom/blob/master/R/lm_tidiers.R
# https://github.com/tidyverse/broom/blob/master/R/lm_tidiers.R#L131
# data.frame.seal

# * logical vector
# * integer vector
# * numeric vector
# * complex vector
# * character vector
# * raw vector
# * list
# * matrix
# * array
# [x] data.frame
# * factor
# * closure function
# * builtin function
# * special function
# * environment
# * null
# * formula
# * expression
# * call
# * pairlist
# * external pointer

# seal <- function(x, ...) {
#   UseMethod("seal")
# }
#
# seal.data.frame <- function(x, ...) {
#   dim(x)
# }
#
# seal(iris)
# # seal(as.matrix(iris))
#
#
#
# # devtools:::print.session_info
# :::print.session_info
# devtools:::print.session_info(devtools::session_info())
# devtools::session_info()
#
# library(sessioninfo)
# session_info()
# lookup::lookup(session_info)
#
# methods("print") %>%
#   grep("session_info", ., value = TRUE)
#
# sessioninfo:::print.session_info
# session_info
# platform_info
#
# clisymbols::symbol$line
# sessioninfo:::rule()
# cli::rule()
# sessioninfo:::rule("Session info")
#
# structure(list(platform = devtools:::platform_info(),
#                packages = devtools:::package_info(pkgs)),
#           class = "session_info")
#
