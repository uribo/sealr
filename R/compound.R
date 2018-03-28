#' Storage R object to a temporary environment
#'
#' @inheritParams design
#' @examples
#' \dontrun{
#' compound(letters)
#' compound(iris)
#'
#' x <- c(1, 3, 5)
#' x %>% compound()
#' }
#' @name compound
compound <- function(x) {
  UseMethod("compound")
}

compound.default <- function(x) {
  e <- new.env()

  assign(".obj", unique(lhs_name(get(
    "x", environment()
  ))), envir = e)
  if (length(e$.obj) == 0) {
    assign(".obj", deparse(as.list(match.call())$x), envir = e)
  }
  return(e)
}

compound.data.frame <- function(x) {
  e <- new.env()

  vars <-
    paste(unique(lhs_name(get(
      "x", environment()
    ))), names(x), sep = "$")
  purrr::walk(1:length(vars), ~
                assign(vars[.x], x[, .x], envir = e))

  assign(".obj", unique(lhs_name(get(
    "x", environment()
  ))), envir = e)
  if (length(e$.obj) == 0) {
    assign(".obj", deparse(as.list(match.call())$x), envir = e)
  }

  return(e)
}
