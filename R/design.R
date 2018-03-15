#' Print an \R{} object test result
#'
#' @param x \R{} object
#' @inheritParams collect
#' @name design
NULL

#' @rdname design
#' @example
#' design_expect_length(letters)
design_expect_length <- function(x) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue("expect_length(
               {x},
               \n",
               x = get("obj", e)),
    rlang::expr_text(length(x)),
    "\n)"
  ))
}

#' @rdname design
#' @example
#' design_expect_range(letters) %>% seal()
design_expect_range <- function(x) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue("expect_equal(
               range({x}),
               \n",
               x = get("obj", e)),
    rlang::expr_text(range(x)),
    "\n)"
  ))
}

#' @rdname design
#' @example
#' design_expect_dim(iris)
design_expect_dim <- function(x) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue("expect_equal(
               dim({x}),
               \n",
               x = get("obj", e)),
    rlang::expr_text(dim(x)),
    "\n)"
  ))
}

#' @rdname design
#' @example
#' x <- c(1, 1, 3, 2)
#' length(x)
#' design_expect_unique(x)
#' design_expect_unique(c("a", "a", "b"))
design_expect_unique <- function(x) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue("expect_equal(
               unique({x}),
               \n",
               x = get("obj", e)),
    rlang::expr_text(unique(x)),
    "\n)"
  ))
}

#' @rdname design
#' @example
#' x <- c(1, NA, 3)
#' desing_expect_missings(x = x)
#' desing_expect_missings(c(1, NA, 3))
desing_expect_missings <- function(x) {
  e <- compound(x)

  as.character(
    glue::glue(
      glue::glue(
        "expect_equal(
      sum(is.na({x})),
      \n",
        x = get("obj", e)
      ),
      rlang::expr_text(sum(is.na(x))),
      "\n)"
    ))
}

#' @rdname design
#' @example
#' my_species <- iris$Species
#' design_expect_levels(my_species)
#' my_species %>% design_expect_levels()
design_expect_levels <- function(x) {
  e <- compound(x)

  as.character(
    glue::glue(
      glue::glue(
        "expect_equal(\n
      levels({x}),
      \n",
        x = get("obj", e)
      ),
      rlang::expr_text(levels(x)),
      "\n)"
    ))
}

#' @rdname design
#' @example
#' my_species <- iris$Species
#' design_expect_nlevels(my_species)
#' my_species %>% design_expect_nlevels()
design_expect_nlevels <- function(x) {
  e <- compound(x)

  as.character(
  glue::glue(
    glue::glue(
      "expect_equal(\n
      nlevels({x}),
      \n",
      x = get("obj", e)
    ),
    rlang::expr_text(nlevels(x)),
    "\n)"
  ))
}

#' @rdname design
#' @example
#' design_expect_nrow(mtcars)
design_expect_nrow <- function(x) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue(
      "expect_equal(
      nrow({x}),
      \n",
      x = get("obj", e)
    ),
    rlang::expr_text(nrow(x)),
    "\n)"
  ))
}

#' @rdname design
#' @example
#' design_expect_ncol(mtcars)
design_expect_ncol <- function(x) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue(
      "expect_equal(
      ncol({x}),
      \n",
      x = get("obj", e)
    ),
    rlang::expr_text(ncol(x)),
    "\n)"
  ))
}

#' @rdname design
#' @example
#' design_expect_names(iris) %>%
#'   seal(clip = FALSE)
design_expect_names <- function(x) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue(
      "expect_named(
      {x},
      \n",
      x = get("obj", e)
    ),
    rlang::expr_text(names(x)),
    "\n)"
  ))
}

#' @rdname design
#' @example
#' design_expect_varclass(iris)
design_expect_varclass <- function(x) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue(
      "expect_equal(
      {x} %>% purrr::map_chr(class) %>% unname(),
      \n",
      x = get("obj", e)
    ),
    rlang::expr_text(x %>%
                       purrr::map_chr(class) %>%
                       unname()),
    "\n)"
  ))
}

#' @rdname design
#' @example
#' design_expect_class(letters)
#' design_expect_class(iris)
design_expect_class <- function(x, environment = NULL) {
  e <- compound(x)

  if (is.null(environment)) {
    env <- .GlobalEnv
  } else {
    env <- environment
  }

  if (isS4(
    glue::evaluate(
      glue::glue(
        "{x}",
        x = get("obj", e)),
      envir = env)
    ) == TRUE) {

    as.character(glue::glue(
      glue::glue(
        "expect_s4_class(
      {x},
      \n",
        x = get("obj", e)
      ),
      rlang::expr_text(class(x)),
      "\n)"
    ))

  } else {
    as.character(glue::glue(
      glue::glue(
        "expect_is(
      {x},
      \n",
        x = get("obj", e)
      ),
      rlang::expr_text(class(x)),
      "\n)"
    ))
  }
}
