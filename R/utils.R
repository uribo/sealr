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

compound <- function(x) {
  e <- new.env()

  e$obj <- unique(lhs_name(get("x", environment())))

  if (length(e$obj) == 0) {
    e$obj <- deparse(as.list(match.call())$x)
  }
  return(e)
}

lhs_obj <- function(x) {

  environment <- name <- mem <- NULL

  load_envs <- search()

  df_objects <- tibble::data_frame(
    environment = load_envs %>%
      purrr::map(
        ~ rep(.x[1], times = length(ls(.x[1])))
      ) %>%
      purrr::flatten_chr(),
    name = load_envs %>%
      purrr::map(
        ~ ls(.x)
      ) %>%
      purrr::flatten_chr(),
    mem = name %>%
      purrr::map_chr(
        ~ lobstr::obj_addr(get(.x))
      ),
    class = name %>%
      purrr::map(
        ~ get(..1)) %>%
      purrr::map(class)
  )

  res <- df_objects %>%
    dplyr::filter(mem == lobstr::obj_addr(x)) %>%
  tidyr::unnest() #%>%
  #dplyr::filter( # FIXME
  #              name != "x")

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
