#' Transcribe \R{} object assert conditions
#'
#' @inheritParams design
#' @param seal which output testthat script
#' @inheritDotParams seal -test
#' @export
#' @examples
#' \dontrun{
#' x <- 1:3L; transcribe(x)
#' transcribe(3.14, seal = FALSE)
#' transcribe(letters, load_testthat = TRUE, ts = FALSE)
#' x <- iris$Species; transcribe(x)
#' transcribe(iris)
#' }
transcribe <- function(x, seal = TRUE, ...) {
  UseMethod("transcribe")
}

transcribe.default <- function(x, seal = TRUE, ...) {
  e <- compound(x)

  rlang::expr_interp(
    paste0(
      "test_that(\"",
      get("obj", e),
      '", {',
      glue::evaluate(glue::glue(
        "design_class({x})",
        x = get("obj", e)
      ),
      envir = e),
      "})"
    )) %>%
    sealing(seal = seal, ...)
}

#' @export
transcribe.numeric <- function(x, seal = TRUE, ...) {
  e <- compound(x)

  rlang::expr_interp(
    paste0(
      "test_that(\"",
      get("obj", e),
      '", {',
      glue::evaluate(glue::glue(
        "design_class({x})",
        x = get("obj", e)
      ),
      envir = e),
      "\n",
      glue::evaluate(glue::glue(
        "design_length({x})",
        x = get("obj", e)
      ),
      envir = e),
      "\n",
      glue::evaluate(glue::glue(
        "design_unique({x})",
        x = get("obj", e)
      ),
      envir = e),
      "\n",
      glue::evaluate(glue::glue(
        "design_range({x})",
        x = get("obj", e)
      ),
      envir = e),
      "})"
    )
  ) %>%
    sealing(seal = seal, ...)
}

#' @export
transcribe.character <- function(x, seal = TRUE, ...) {
  e <- compound(x)

  rlang::expr_interp(
    paste0(
      "test_that(\"",
      get("obj", e),
      '", {',
      glue::evaluate(glue::glue(
        "design_class({x})",
        x = get("obj", e)
      ),
      envir = e),
      "\n",
      glue::evaluate(glue::glue(
        "design_length({x})",
        x = get("obj", e)
      ),
      envir = e),
      "\n",
      glue::evaluate(glue::glue(
        "design_unique({x})",
        x = get("obj", e)
      ),
      envir = e),
      "\n",
      glue::evaluate(glue::glue(
        "design_range({x})",
        x = get("obj", e)
      ),
      envir = e),
      "})"
    )
  ) %>%
    sealing(seal = seal, ...)
}

#' @export
transcribe.factor <- function(x, seal = TRUE, ...) {
  e <- compound(x)

  rlang::expr_interp(
    paste0(
      "test_that(\"",
      get("obj", e),
      '", {',
      glue::evaluate(glue::glue(
        "design_class({x})",
        x = get("obj", e)
      ),
      envir = e),
      "\n",
      glue::evaluate(glue::glue(
        "design_length({x})",
        x = get("obj", e)
      ),
      envir = e),
      "\n",
      glue::evaluate(glue::glue(
        "design_levels({x})",
        x = get("obj", e)
      ),
      envir = e),
      "\n",
      glue::evaluate(glue::glue(
        "design_nlevels({x})",
        x = get("obj", e)
      ),
      envir = e),
      "})"
    )) %>%
    sealing(seal = seal, ...)
}

#' @export
transcribe.list <- function(x, seal = TRUE, ...) {
  e <- compound(x)

  rlang::expr_interp(paste0(
    "test_that(\"",
    get("obj", e),
    '", {',
    glue::evaluate(glue::glue(
      "design_class({x})",
      x = get("obj", e)
      ),
      envir = e),
    "\n",
    glue::evaluate(glue::glue(
      "design_length({x})",
      x = get("obj", e)
    ),
    envir = e),
    "\n",
    glue::evaluate(glue::glue("design_names({x})",
                              x = get("obj", e)),
                   envir = e),
    "})"
  )) %>%
    sealing(seal = seal, ...)
}

#' @export
transcribe.matrix <- function(x, seal = TRUE, ...) {
  e <- compound(x)

  rlang::expr_interp(paste0(
    "test_that(\"",
    get("obj", e),
    '", {',
    glue::evaluate(glue::glue(
      "design_class({x})",
      x = get("obj", e)
    ),
    envir = e),
    "\n",
    glue::evaluate(glue::glue(
      "design_dim({x})",
      x = get("obj", e)
    ),
    envir = e),
    "\n",
    glue::evaluate(
      glue::glue("design_dimnames({x})",
                 x = get("obj", e)),
      envir = e),
    "})"
  )) %>%
    sealing(seal = seal, ...)
}

#' @export
transcribe.table <- function(x, seal = TRUE, ...) {
  e <- compound(x)

  rlang::expr_interp(paste0(
    "test_that(\"",
    get("obj", e),
    '", {',
    glue::evaluate(glue::glue(
      "design_class({x})",
      x = get("obj", e)
    ),
    envir = e),
    "\n",
    glue::evaluate(glue::glue(
      "design_dim({x})",
      x = get("obj", e)
    ),
    envir = e),
    "\n",
    glue::evaluate(
      glue::glue("design_dimnames({x})",
                 x = get("obj", e)),
      envir = e),
    "})"
  )) %>%
    sealing(seal = seal, ...)
}

#' @export
transcribe.data.frame <- function(x, seal = TRUE, ...) {
  e <- compound(x)

  rlang::expr_interp(
    paste0(
      "test_that(\"",
      get("obj", e),
      '", {',
      glue::evaluate(glue::glue("design_class({x})",
                                x = get("obj", e)),
                     envir = e),
      "\n",
      glue::evaluate(glue::glue("design_dim({x})",
                                x = get("obj", e)),
                     envir = e),
      "\n",
      glue::evaluate(glue::glue("design_names({x})",
                                x = get("obj", e)),
                     envir = e),
      "\n",
      glue::evaluate(glue::glue("design_varclass({x})",
                                x = get("obj", e)),
                     envir = e),
      "})"
    )
  ) %>%
    sealing(seal = seal, ...)
}
