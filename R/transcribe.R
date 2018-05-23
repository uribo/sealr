#' Transcribe \R{} object assert conditions
#'
#' @inheritParams design
#' @param desc test name. Default adopt object name.
#' @param detail `Logical`. Only data frame.
#' If you chose *TRUE*, to make a detailed record for each variables in data frame.
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
transcribe <- function(x, desc = NULL, seal = TRUE, detail = FALSE, ...) {
  UseMethod("transcribe")
}

#' @export
transcribe.default <- function(x, desc = NULL, seal = TRUE, ...) {
  e <- compound(x)

  rlang::expr_interp(
    paste0(
      "test_that(\"",
      label(e, desc),
      '", {',
      eval(parse(text = glue::glue(
        "design_class({x})",
        x = get(".obj", e)
      )),
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
      label(e, desc),
      '", {',
      eval(parse(text = glue::glue(
        "design_class({x})",
        x = get(".obj", e)
      )),
      envir = e),
      "\n",
      eval(parse(text = glue::glue(
        "design_length({x})",
        x = get(".obj", e)
      )),
      envir = e),
      "\n",
      eval(parse(text = glue::glue(
        "design_unique({x})",
        x = get(".obj", e)
      )),
      envir = e),
      "\n",
      eval(parse(text = glue::glue(
        "design_range({x})",
        x = get(".obj", e)
      )),
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
      label(e, desc),
      '", {',
      eval(parse(text = glue::glue(
        "design_class({x})",
        x = get(".obj", e)
      )),
      envir = e),
      "\n",
      eval(parse(text = glue::glue(
        "design_length({x})",
        x = get(".obj", e)
      )),
      envir = e),
      "\n",
      eval(parse(text = glue::glue(
        "design_unique({x})",
        x = get(".obj", e)
      )),
      envir = e),
      "\n",
      eval(parse(text = glue::glue(
        "design_range({x})",
        x = get(".obj", e)
      )),
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
      label(e, desc),
      '", {',
      eval(parse(text = glue::glue(
        "design_class({x})",
        x = get(".obj", e)
      )),
      envir = e),
      "\n",
      eval(parse(text = glue::glue(
        "design_length({x})",
        x = get(".obj", e)
      )),
      envir = e),
      "\n",
      eval(parse(text = glue::glue(
        "design_levels({x})",
        x = get(".obj", e)
      )),
      envir = e),
      "\n",
      eval(parse(text = glue::glue(
        "design_nlevels({x})",
        x = get(".obj", e)
      )),
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
    label(e, desc),
    '", {',
    eval(parse(text = glue::glue(
      "design_class({x})",
      x = get(".obj", e)
    )),
    envir = e),
    "\n",
    eval(parse(text = glue::glue(
      "design_length({x})",
      x = get(".obj", e)
    )),
    envir = e),
    "\n",
    eval(parse(text = glue::glue(
      "design_names({x})",
      x = get(".obj", e)
    )),
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
    label(e, desc),
    '", {',
    eval(parse(text = glue::glue(
      "design_class({x})",
      x = get(".obj", e)
    )),
    envir = e),
    "\n",
    eval(parse(text = glue::glue(
      "design_dim({x})",
      x = get(".obj", e)
    )),
    envir = e),
    "\n",
    eval(parse(text = glue::glue(
      "design_dimnames({x})",
      x = get(".obj", e)
    )),
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
    label(e, desc),
    '", {',
    eval(parse(text = glue::glue(
      "design_class({x})",
      x = get(".obj", e)
    )),
    envir = e),
    "\n",
    eval(parse(text = glue::glue(
      "design_dim({x})",
      x = get(".obj", e)
    )),
    envir = e),
    "\n",
    eval(parse(text = glue::glue(
      "design_dimnames({x})",
      x = get(".obj", e)
    )),
    envir = e),
    "})"
  )) %>%
    sealing(seal = seal, ...)
}

#' @export
transcribe.data.frame <- function(x,
                                  desc = NULL,
                                  seal = TRUE,
                                  detail = FALSE,
                                  ...) {
  e <- compound(x)

  design <- paste0(
    eval(parse(text = glue::glue(
      "design_class({x})",
      x = get(".obj", e)
    )),
    envir = e),
    "\n",
    eval(parse(text = glue::glue(
      "design_dim({x})",
      x = get(".obj", e)
    )),
    envir = e),
    "\n",
    eval(parse(text = glue::glue(
      "design_names({x})",
      x = get(".obj", e)
    )),
    envir = e),
    "\n",
    eval(parse(text = glue::glue(
      "design_varclass({x})",
      x = get(".obj", e)
    )),
    envir = e)
  )

  test <- rlang::expr_interp(paste0(
    "test_that(\"",
    label(e, desc),
    '", {',
    dplyr::if_else(
      rlang::is_true(detail),
      paste(
        design,
        purrr::map(1:length(ls(e)),
                   ~ .design_df_details(ls(e)[.x])) %>%
          purrr::reduce(paste, sep = "\n")
        ,
        sep = "\n"
      ),
      design
    ),
    "})"
  ))

  sealing(test, seal = seal, ...)
}
