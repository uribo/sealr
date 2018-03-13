#' Transcribe \R{} object assert conditions
#'
#' @inheritParams design
#' @export
#' @examples
#' \dontrun{
#' x <- 1:3L; transcribe(x) %>% seal()
#' transcribe(3.14) %>% seal()
#' transcribe(letters) %>% seal()
#' x <- iris$Species; transcribe(x) %>% seal()
#' transcribe(iris) %>% seal()
#' }
transcribe <- function(x) {
  UseMethod("transcribe")
}

#' @export
transcribe.numeric <- function(x) {
  object_name <- as.list(match.call())$x

  rlang::expr_interp(
    paste0(
      'test_that("',
      object_name,
      '", {',
      rlang::eval_tidy(match.call(
        get,
        call("design_expect_class", x = object_name)
      )),
      "\n",
      rlang::eval_tidy(match.call(
        get,
        call("design_expect_length", x = object_name)
      )),
      "\n",
      rlang::eval_tidy(match.call(
        get,
        call("design_expect_unique_length", x = object_name)
      )),
      "\n",
      rlang::eval_tidy(match.call(
        get,
        call("design_expect_range", x = object_name)
      )),
      "})"
    )
  )

}

#' @export
transcribe.character <- function(x) {
  object_name <- as.list(match.call())$x

  rlang::expr_interp(
    paste0(
      'test_that("',
      object_name,
      '", {',
      rlang::eval_tidy(match.call(
        get,
        call("design_expect_class", x = object_name)
      )),
      "\n",
      rlang::eval_tidy(match.call(
        get,
        call("design_expect_length", x = object_name)
      )),
      "\n",
      rlang::eval_tidy(match.call(
        get,
        call("design_expect_unique_length", x = object_name)
      )),
      "\n",
      rlang::eval_tidy(match.call(
        get,
        call("design_expect_range", x = object_name)
      )),
      "})"
    )
  )
}

#' @export
transcribe.factor <- function(x) {
  object_name <- as.list(match.call())$x

  rlang::expr_interp(
    paste0(
      'test_that("',
      object_name,
      '", {',
      rlang::eval_tidy(match.call(
        get,
        call("design_expect_class", x = object_name)
      )),
      "\n",
      rlang::eval_tidy(match.call(
        get,
        call("design_expect_length", x = object_name)
      )),
      "\n",
      rlang::eval_tidy(match.call(
        get,
        call("design_expect_levels", x = object_name)
      )),
      "\n",
      rlang::eval_tidy(match.call(
        get,
        call("design_expect_nlevels", x = object_name)
      )),
      "})"
    )
  )
}

#' @export
transcribe.data.frame <- function(x) {
  object_name <- as.list(match.call())$x

  rlang::expr_interp(
    paste0(
      'test_that("',
      object_name,
      '", {',
      rlang::eval_tidy(match.call(
        get,
        call("design_expect_class", x = object_name)
      )),
      "\n",
      rlang::eval_tidy(match.call(
        get,
        call("design_expect_dim", x = object_name)
      )),
      "\n",
      rlang::eval_tidy(match.call(
        get,
        call("design_expect_names", x = object_name)
      )),
      "\n",
      rlang::eval_tidy(match.call(
        get,
        call("design_expect_varclass", x = object_name)
      )),
      "})"
    )
  )

}
