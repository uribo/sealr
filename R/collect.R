#' Collect \R{} object
#'
#' @details
#'
#' * filter_context
#'
#' @param context Types of \R{} object (`character`)
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

collect_objects <- function() {
  target <- ls(name = .GlobalEnv)

  df <- tibble::data_frame(
    name = target,
    eval = target %>%
      purrr::map(get),
    class = eval %>%
      purrr::map(class)) %>%
    dplyr::mutate(
      class = purrr::pmap_chr(., ~ paste(..3, collapse = ", "))) %>%
    tidyr::separate_rows(col = class, into = class)

  return(df)

}

#' @rdname collect
#' @export
filter_context <- function(context = "function") {

  collect_objects() %>%
    dplyr::filter(class == context)
}
