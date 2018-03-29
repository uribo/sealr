sealr_timestamp <- function(quiet = FALSE) {
    utils::timestamp(
      stamp = Sys.Date(),
      prefix = paste0("#' ",
                      clisymbols::symbol$info,
                      ": Labeling on "),
      suffix = paste0(
        " by the sealr package (v",
        as.character(utils::packageVersion("sealr")),
        ")"
      ),
      quiet = quiet
    )
}

obj_eval <- function(df) {
  df %>%
    dplyr::mutate(
      eval = purrr::pmap(.,
                         ~ get(..2)
      )
    )
}

#' Catch lhs object which exclude function
#'
#' @noRd
lhs_obj <- function(x) {
  environment <- name <- hash <- NULL

  load_envs <- search()

  res <- tibble::data_frame(
    environment = load_envs %>%
      purrr::map(~ rep(.x[1], times = length(ls(
        .x[1]
      )))) %>%
      purrr::flatten_chr(),
    name = load_envs %>%
      purrr::map(~ ls(.x)) %>%
      purrr::flatten_chr(),
    class = name %>%
      purrr::map(~ get(..1)) %>%
      purrr::map(class)
  ) %>%
    tidyr::unnest() %>%
    dplyr::filter(class != "function") %>%
    dplyr::mutate(hash = name %>%
                    purrr::map_chr(~ digest::digest(get(.x), algo = "sha256"))) %>%
    dplyr::filter(hash == digest::digest(x, algo = "sha256"))

  if (nrow(res) == 0) {
    rlang::inform("The given object is not stored in any environment.")
  }

  return(res)

}

lhs_name <- function(x) {

  res <- lhs_obj(x)$name

    # FIXME
  if (length(res) > 1 && sum(grepl("^x$", res)) >= 1) {
    res <- res[!res %in% "x"]
  }

  return(res)
}

#' Generate test_that desc text
#'
#' @noRd
label <- function(e, desc) {
  dplyr::if_else(is.null(desc),
                 paste("check", get(".obj", e), "statement"),
                 desc)
}

sealing <- function(x, seal = FALSE, ...) {
  if (rlang::is_true(seal)) {
    seal(x, ...)
  } else {
    x
  }
}
