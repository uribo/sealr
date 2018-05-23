#' Print an \R{} object test result
#'
#' @param x \R{} object
#' @inheritDotParams seal -test
#' @param environment which environment (work space) to search the available objects
#' @name design
NULL

#' @rdname design
#' @examples
#' design_length(letters)
#' @export
design_length <- function(x, ...) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue("expect_length(
               {x},
               \n",
               x = get(".obj", e)),
    rlang::expr_text(length(x)),
    "\n)"
  )) %>%
    sealing(...)
}

#' @rdname design
#' @examples
#' design_range(letters)
#' x <- c(1, NA, 3, 5)
#' design_range(x)
#' @export
design_range <- function(x, ...) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue("expect_equal(
               range({x}, na.rm = TRUE),
               \n",
               x = get(".obj", e)),
    rlang::expr_text(range(x, na.rm = TRUE)),
    "\n)"
  )) %>%
    sealing(...)
}

#' @rdname design
#' @examples
#' design_dim(iris)
#' @export
design_dim <- function(x, ...) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue("expect_equal(
               dim({x}),
               \n",
               x = get(".obj", e)),
    rlang::expr_text(dim(x)),
    "\n)"
  )) %>%
    sealing(...)
}

#' @rdname design
#' @examples
#' x <- c(1, 1, 3, 2)
#' length(x)
#' design_unique(x)
#' design_unique(c("a", "a", "b"), seal = TRUE)
#' @export
design_unique <- function(x, ...) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue("expect_equal(
               unique({x}),
               \n",
               x = get(".obj", e)),
    rlang::expr_text(unique(x)),
    "\n)"
  )) %>%
    sealing(...)
}

#' @rdname design
#' @examples
#' x <- c(1, NA, 3)
#' design_missings(x = x)
#' design_missings(c(1, NA, 3))
#' @export
design_missings <- function(x, ...) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue("expect_equal(
               sum(is.na({x})),
               \n",
               x = get(".obj", e)),
    rlang::expr_text(sum(is.na(x))),
    "\n)"
  )) %>%
    sealing(...)
}

#' @rdname design
#' @examples
#' my_species <- iris$Species
#' design_levels(my_species)
#' my_species %>% design_levels()
#' @export
design_levels <- function(x, ...) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue("expect_equal(\n
               levels({x}),
               \n",
               x = get(".obj", e)),
    rlang::expr_text(levels(x)),
    "\n)"
  )) %>%
    sealing(...)
}

#' @rdname design
#' @examples
#' my_species <- iris$Species
#' design_nlevels(my_species, seal = FALSE, ts = FALSE)
#' my_species %>% design_nlevels()
#' @export
design_nlevels <- function(x, ...) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue("expect_equal(\n
               nlevels({x}),
               \n",
               x = get(".obj", e)),
    rlang::expr_text(nlevels(x)),
    "\n)"
  )) %>%
    sealing(...)
}

#' @rdname design
#' @examples
#' design_nrow(mtcars)
#' @export
design_nrow <- function(x, ...) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue("expect_equal(
               nrow({x}),
               \n",
               x = get(".obj", e)),
    rlang::expr_text(nrow(x)),
    "\n)"
  )) %>%
    sealing(...)
}

#' @rdname design
#' @examples
#' design_ncol(mtcars)
#' @export
design_ncol <- function(x, ...) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue("expect_equal(
               ncol({x}),
               \n",
               x = get(".obj", e)),
    rlang::expr_text(ncol(x)),
    "\n)"
  )) %>%
    sealing(...)
}

#' @rdname design
#' @examples
#' design_names(iris)
#' @export
design_names <- function(x, ...) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue("expect_named(
               {x},
               \n",
               x = get(".obj", e)),
    rlang::expr_text(names(x)),
    "\n)"
  )) %>%
    sealing(...)
}

#' @rdname design
#' @export
design_dimnames <- function(x, ...) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue("expect_equal(
               dimnames({x}),
               \n",
               x = get(".obj", e)),
    rlang::expr_text(dimnames(x)),
    "\n)"
  )) %>%
    sealing(...)
}

#' @rdname design
#' @examples
#' design_varclass(iris)
#' @export
design_varclass <- function(x, ...) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue(
      "expect_equal(
      {x} %>% purrr::map(class) %>% unname(),
      \n",
      x = get(".obj", e)
    ),
    rlang::expr_text(x %>%
                       purrr::map(class) %>%
                       unname()),
    "\n)"
  )) %>%
    sealing(...)
}

#' @rdname design
#' @examples
#' design_class(letters)
#' design_class(iris)
#' @export
design_class <- function(x, environment = NULL, ...) {
  e <- compound(x)

  if (is.null(environment)) {
    env <- .GlobalEnv
  } else {
    env <- environment
  }

  if (isS4(eval(parse(text = glue::glue("{x}",
                                        x = get(".obj", e))), envir = env)) == TRUE) {
    res <- as.character(glue::glue(
      glue::glue("expect_s4_class(
                 {x},
                 \n",
                 x = get(".obj", e)),
      rlang::expr_text(class(x)),
      "\n)"
    ))

  } else {
    res <- as.character(glue::glue(
      glue::glue("expect_is(
                 {x},
                 \n",
                 x = get(".obj", e)),
      rlang::expr_text(class(x)),
      "\n)"
    ))
  }
  sealing(res, ...)
}

#' @rdname design
#' @examples
#' design_obj_size(iris,
#'                 seal = TRUE,
#'                 load_testthat = TRUE,
#'                 clip = FALSE)
#' design_obj_size(letters)
#' @export
design_obj_size <- function(x, ...) {

  e <- compound(x)

  as.character(glue::glue(
    glue::glue("expect_equal(
               lobstr::obj_size({x}),
               \n",
               x = get(".obj", e)),
    rlang::expr_text(lobstr::obj_size(x)),
    "\n)"
  )) %>%
    sealing(...)
}

#' @noRd
.design_df_details <- function(var) {

  obj <- eval(parse(text = var), envir = .GlobalEnv)

  test_common <- glue::glue(
    "expect_equal(
    sum(is.na({x})),\n",
    rlang::expr_text(
      sum(is.na(
        eval(parse(text = var), envir = .GlobalEnv)))),
    ")",
    x = rlang::sym(var)
  )

  test_specific <- if (is.numeric(obj)) {
    glue::glue(
      "expect_equal(
      range({x}, na.rm = TRUE),\n",
      rlang::expr_text(
        range(
          eval(parse(text = var), envir = .GlobalEnv), na.rm = TRUE)),
      ")",
      x = rlang::sym(var)
    )
  } else if (is.factor(obj)) {
    glue::glue(
      "expect_equal(
      {x},\n",
      rlang::expr_text(
        levels(
          eval(parse(text = var), envir = .GlobalEnv))),
      ")",
      x = rlang::sym(var)
    )
  } else if (is.character(obj)) {
    glue::glue(
      "expect_equal(
      length(unique({x})),\n",
      rlang::expr_text(
        length(unique(
          eval(parse(text = var), envir = .GlobalEnv)))),
      ")",
      x = rlang::sym(var)
    )
  } else {
    glue::glue(
      "expect_equal(
      {x},\n",
      rlang::expr_text(
        length(
          eval(parse(text = var), envir = .GlobalEnv))),
      ")",
      x = rlang::sym(var)
    )
  }

  paste(test_common,
        test_specific,
        sep = "\n")
}
