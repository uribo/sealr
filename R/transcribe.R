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
transcribe <- function(x, environment = NULL) {
  UseMethod("transcribe")
}

#' @export
transcribe.numeric <- function(x, environment = NULL) {
  e <- compound(x)
  if (is.null(environment)) {
    env <- .GlobalEnv
  } else {
    env <- environment
  }

  rlang::expr_interp(
    paste0(
      "test_that(\"",
      get("obj", e),
      '", {',
      glue::evaluate(glue::glue(
        "design_expect_class({x})",
        x = get("obj", e)
      ),
      envir = e),
      "\n",
      glue::evaluate(glue::glue(
        "design_expect_length({x})",
        x = get("obj", e)
      ),
      envir = e),
      "\n",
      glue::evaluate(glue::glue(
        "design_expect_unique({x})",
        x = get("obj", e)
      ),
      envir = e),
      "\n",
      glue::evaluate(glue::glue(
        "design_expect_range({x})",
        x = get("obj", e)
      ),
      envir = e),
      "})"
    )
  )
}

#' @export
transcribe.character <- function(x, environment = NULL) {
  e <- compound(x)
  if (is.null(environment)) {
    env <- .GlobalEnv
  } else {
    env <- environment
  }

  rlang::expr_interp(
    paste0(
      "test_that(\"",
      get("obj", e),
      '", {',
      glue::evaluate(glue::glue(
        "design_expect_class({x})",
        x = get("obj", e)
      ),
      envir = e),
      "\n",
      glue::evaluate(glue::glue(
        "design_expect_length({x})",
        x = get("obj", e)
      ),
      envir = e),
      "\n",
      glue::evaluate(glue::glue(
        "design_expect_unique({x})",
        x = get("obj", e)
      ),
      envir = e),
      "\n",
      glue::evaluate(glue::glue(
        "design_expect_range({x})",
        x = get("obj", e)
      ),
      envir = e),
      "})"
    )
  )
}

#' @export
transcribe.factor <- function(x, environment = NULL) {
  e <- compound(x)
  if (is.null(environment)) {
    env <- .GlobalEnv
  } else {
    env <- environment
  }

  rlang::expr_interp(
    paste0(
      "test_that(\"",
      get("obj", e),
      '", {',
      glue::evaluate(glue::glue(
        "design_expect_class({x})",
        x = get("obj", e)
      ),
      envir = e),
      "\n",
      glue::evaluate(glue::glue(
        "design_expect_length({x})",
        x = get("obj", e)
      ),
      envir = e),
      "\n",
      glue::evaluate(glue::glue(
        "design_expect_levels({x})",
        x = get("obj", e)
      ),
      envir = e),
      "\n",
      glue::evaluate(glue::glue(
        "design_expect_nlevels({x})",
        x = get("obj", e)
      ),
      envir = e),
      "})"
    ))
}

#' @export
transcribe.data.frame <- function(x, environment = NULL) {
  e <- compound(x)
  if (is.null(environment)) {
    env <- .GlobalEnv
  } else {
    env <- environment
  }

  rlang::expr_interp(
    paste0(
      "test_that(\"",
      get("obj", e),
      '", {',
      glue::evaluate(glue::glue(
        "design_expect_class({x})",
        x = get("obj", e)
      ),
      envir = e),
      "\n",
      glue::evaluate(glue::glue(
        "design_expect_dim({x})",
        x = get("obj", e)
      ),
      envir = e),
      "\n",
      glue::evaluate(glue::glue(
        "design_expect_names({x})",
        x = get("obj", e)
      ),
      envir = e),
      "\n",
      glue::evaluate(glue::glue(
        "design_expect_varclass({x})",
        x = get("obj", e)
      ),
      envir = e),
      "})"
    ))

}
