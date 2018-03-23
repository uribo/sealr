#' Transcribe \R{} object assert conditions
#'
#' @inheritParams design
#' @param desc test name. Default adopt object name.
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
transcribe <- function(x, desc = NULL, seal = TRUE, ...) {
  UseMethod("transcribe")
}

transcribe.default <- function(x, desc = NULL, seal = TRUE, ...) {
  e <- compound(x)

  rlang::expr_interp(
    paste0(
      "test_that(\"",
      dplyr::if_else(is.null(desc), get("obj", e), desc),
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
transcribe.numeric <- function(x, desc = NULL, seal = TRUE, ...) {
  e <- compound(x)

  rlang::expr_interp(
    paste0(
      "test_that(\"",
      dplyr::if_else(is.null(desc), get("obj", e), desc),
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
transcribe.character <- function(x, desc = NULL, seal = TRUE, ...) {
  e <- compound(x)

  rlang::expr_interp(
    paste0(
      "test_that(\"",
      dplyr::if_else(is.null(desc), get("obj", e), desc),
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
transcribe.factor <- function(x, desc = NULL, seal = TRUE, ...) {
  e <- compound(x)

  rlang::expr_interp(
    paste0(
      "test_that(\"",
      dplyr::if_else(is.null(desc), get("obj", e), desc),
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
transcribe.list <- function(x, desc = NULL, seal = TRUE, ...) {
  e <- compound(x)

  rlang::expr_interp(paste0(
    "test_that(\"",
    dplyr::if_else(is.null(desc), get("obj", e), desc),
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
transcribe.matrix <- function(x, desc = NULL, seal = TRUE, ...) {
  e <- compound(x)

  rlang::expr_interp(paste0(
    "test_that(\"",
    dplyr::if_else(is.null(desc), get("obj", e), desc),
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
transcribe.table <- function(x, desc = NULL, seal = TRUE, ...) {
  e <- compound(x)

  rlang::expr_interp(paste0(
    "test_that(\"",
    dplyr::if_else(is.null(desc), get("obj", e), desc),
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
transcribe.data.frame <- function(x, desc = NULL, seal = TRUE, ...) {
  e <- compound(x)

  rlang::expr_interp(
    paste0(
      "test_that(\"",
      dplyr::if_else(is.null(desc), get("obj", e), desc),
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
