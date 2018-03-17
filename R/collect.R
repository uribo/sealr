#' Collect \R{} object
#'
#' @details
#'
#' * ls_objects
#'
#' @param class Class of \R{} object (`character`)
#' @param environment which environment (work space) to search the available objects
#' @param pkgs The name of a package such as `package:<PACKAGE_NAME>`
#' @param nms attached packages names
#' @param eval which include evaluate value
#' @param ... Further arguments
#'
#' @name collect
#' @examples
#' \dontrun{
#' Return objects in .GlobaEnv
#' my_iris <- iris
#' ls_objects()
#' # Filter by object class
#' ls_objects("data.frame")
#' # Storage a evaluate value
#' ls_objects("data.frame", eval = TRUE)
#'
#' # Filter object class and environment
#' ls_objects("data.frame",
#'            nms = TRUE,
#'            pkgs = "package:datasets")
#'
#' library(dplyr)
#' ls_objects(c("function", "tbl"),
#'            nms = TRUE,
#'            pkgs = "package:dplyr")
#'
#' e <- rlang::env(my_data1 = iris, my_data2 = mtcars)
#' ls_objects(environment = "e", nms = FALSE)
#' }
NULL

. <- name <- NULL

#' @rdname collect
global_objects <- function() {

  df <- tibble::tibble(
    name = rlang::env_names(rlang::global_env()),
    class = rlang::env_get_list(.GlobalEnv, name) %>%
      purrr::map(class)
  ) %>%
    dplyr::mutate(environment = ".GlobalEnv")

  env_env <- function(envs) {
    envs %>%
      purrr::map_dfr(
        ~ tibble::tibble(
          name = rlang::env_names(rlang::env_get(.GlobalEnv, .x)),
          class = rlang::env_get_list(rlang::env_get(.GlobalEnv, .x), name) %>%
            purrr::map(class)
        ) %>%
          dplyr::mutate(environment = .x)
      )
  }

  df_envs <- df %>%
    dplyr::filter(class == "environment") %>%
    magrittr::use_series(name) %>%
    env_env()

  dplyr::bind_rows(df, df_envs) %>%
    dplyr::select(environment, name, class)

}

#' @rdname collect
nms_objects <- function(pkgs = NULL, ...) {

  nms <- rlang::scoped_names()

  nms <- grep("^(package|tools)", nms, value = TRUE)

  if (!is.null(pkgs)) {
    nms <- nms[nms %in% pkgs]
  }

  tibble::data_frame(
    environment = nms %>%
      purrr::map(~ rep(.x[1], times = length(ls(
        .x[1]
      )))) %>%
      purrr::flatten_chr(),
    name = nms %>%
      purrr::map(~ ls(.x)) %>%
      purrr::flatten_chr(),
    class = name %>%
      purrr::map(~ get(..1, pos  = environment)) %>%
      purrr::map(class)
  )
}

#' @rdname collect
#' @export
ls_objects <- function(class = NULL,
                       environment = ".GlobalEnv",
                       eval = FALSE,
                       nms = FALSE, ...) {

  environment <- rlang::quo_expr(environment)
  class <- rlang::quo_expr(class)

  df_objects <- global_objects()

  if (!is.null(environment)) {
    df_objects <-
      df_objects %>%
      dplyr::filter(environment %in% !!c(environment))
  }

  if (isTRUE(nms)) {
    df_objects <-
      df_objects %>%
      dplyr::bind_rows(nms_objects(...))
  }

  df_objects <-
    df_objects %>%
    dplyr::mutate(class = purrr::pmap_chr(., ~ paste(..3, collapse = ", "))) %>%
    tidyr::separate_rows(col = class, into = class)

  if (!is.null(class)) {
    df_objects <-
      df_objects %>%
      dplyr::filter(class %in% !!c(class))
  }

  if (isTRUE(eval)) {
    df_objects <-
      df_objects %>%
      obj_eval()
  }

  if (nrow(df_objects) == 0) {
    return(rlang::inform("The given environment is not stored any objects."))
  }

  return(df_objects)
}
