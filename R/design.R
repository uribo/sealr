#' Print an \R{} object test result
#'
#' @param x \R{} object
#' @name design
NULL

#' @rdname design
#' @example
#' design_expect_length(letters)
design_expect_length <- function(x) {

  object_name <- deparse(as.list(match.call())$x)

  paste0(
    "expect_length(\n",
    paste(
      object_name,
      ",\n",
      rlang::expr_text(length(x))),
    ")"
  )
}


#' @rdname design
#' @example
#' design_expect_range(letters) %>% seal()
design_expect_range <- function(x) {

  object_name <- deparse(as.list(match.call())$x)

  paste0("expect_equal(\n",
         paste("range(",
               object_name,
               "),\n",
               rlang::expr_text(range(x))),
         ")")
}

#' @rdname design
#' @example
#' design_expect_dim(iris)
design_expect_dim <- function(x) {

  object_name <- deparse(as.list(match.call())$x)

  paste0(
    "expect_equal(\n",
    paste("dim(",
          object_name,
          "),\n"),
    rlang::expr_text(dim(x)),
    ")"
  )
}

#' @rdname design
#' @example
#' x <- c(1, 1, 3, 2)
#' length(x)
#' design_expect_unique_length(x)
design_expect_unique_length <- function(x) {

  object_name <- deparse(as.list(match.call())$x)

  paste0(
    "expect_length(\n",
    object_name,
    ",\n",
    rlang::expr_text(length(unique(x))),
    ")")
}

#' @rdname design
design_expect_levels <- function(x) {

  object_name <- deparse(as.list(match.call())$x)

  paste0(
    "expect_equal(\nlevels(",
    object_name,
    "),\n",
    rlang::expr_text(levels(x)),
    ")")
}

#' @rdname design
design_expect_nlevels <- function(x) {

  object_name <- deparse(as.list(match.call())$x)

  paste0(
    "expect_equal(\nnlevels(",
    object_name,
    "),\n",
    rlang::expr_text(nlevels(x)),
    ")")
}

#' @rdname design
#' @example
#' design_expect_nrow(mtcars)
design_expect_nrow <- function(x) {

  object_name <- deparse(as.list(match.call())$x)

  paste0(
    "expect_equal(\nnrow(",
    object_name,
    "),\n",
    rlang::expr_text(nrow(x)),
    ")")
}

#' @rdname design
#' @example
#' design_expect_ncol(mtcars)
design_expect_ncol <- function(x) {

  object_name <- deparse(as.list(match.call())$x)

  paste0(
    "expect_equal(\nncol(",
    object_name,
    "),\n",
    rlang::expr_text(ncol(x)),
    ")")
}

#' @rdname design
#' @example
#' design_expect_names(iris) %>% seal()
design_expect_names <- function(x) {

  object_name <- deparse(as.list(match.call())$x)

  paste0(
    "expect_named(\n",
    object_name,
    ",\n",
    rlang::expr_text(names(x)),
    ")")
}

#' @rdname design
#' @example
#' design_expect_varclass(iris)
design_expect_varclass <- function(x) {

  object_name <- deparse(as.list(match.call())$x)

  paste0("expect_equal(\n",
         object_name,
         "%>% purrr::map_chr(class) %>% unname(),\n",
         rlang::expr_text(x %>%
                            purrr::map_chr(class) %>% unname()),
         ")"
  )
}

#' @rdname design
#' @example
#' design_expect_class(letters)
#' design_expect_class(iris)
design_expect_class <- function(x) {

  object_name <- deparse(as.list(match.call())$x)

  if (isS4(x) == TRUE) {

    paste0("expect_s4_class(\n",
           object_name,
           ",\n",
           rlang::expr_text(class(x)),
           ")")

  } else {

    paste0("expect_is(\n",
           object_name,
           ",\n",
           rlang::expr_text(class(x)),
           ")")

  }
}
