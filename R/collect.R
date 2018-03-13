#' Collect \R{} object
#'
#' @details
#'
#' * filter_context
#'
#' @param context Types of \R{} object (`character`)
#' @param environment which environment (work space) to search the available objects
#' @param ... Further arguments
#'
#' @name collect
#' @examples
#' \dontrun{
#' my_iris <- iris
#' filter_context("data.frame") %>%
#'   purrr::pmap(~ dim(..2))
#'
#' my_mtcars <- mtcars
#' filter_context("data.frame") %>%
#' purrr::pmap(~ names(..2))
#' }
NULL

#' @rdname collect
collect_objects <- function(environment = NULL, ...) {

  . <- NULL

  if (is.null(environment)) {
    env <- .GlobalEnv
  } else {
    env <- environment
  }
    target <- ls(name = env)

  df <- tibble::data_frame(
    name = target,
    eval = target %>%
      purrr::map(~ get(..1, envir = env)),
    class = eval %>%
      purrr::map(class)) %>%
    dplyr::mutate(
      class = purrr::pmap_chr(., ~ paste(..3, collapse = ", "))) %>%
    tidyr::separate_rows(col = class, into = class)

  return(df)

}

#' @rdname collect
#' @export
filter_context <- function(context = "function", ...) {

  collect_objects(...) %>%
    dplyr::filter(class == context)
}
