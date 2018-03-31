#' Put seal
#'
#' @noRd
mask_candidate_lines <- function(context) {
  # nocov start
  which_lines_candidate <-
    grep("(design_|transcribe)", context$contents, value = FALSE)

  grep("(design_|transcribe)", context$contents, value = TRUE) %>%
    purrr::set_names(which_lines_candidate) %>%
    gsub("#.+", "", .) %>%
    grep("\"", ., value = TRUE, invert = TRUE) %>%
    grep(".+", ., value = TRUE) %>%
    names() %>%
    as.numeric()
  # nocov end
}

#' @noRd
put <- function(all = FALSE) {
  # nocov start
  context <-
    rstudioapi::getSourceEditorContext()

  is_console <-
    grepl("#console", rstudioapi::getActiveDocumentContext()$id)

  which_lines <-
    mask_candidate_lines(context)

  if (rlang::is_false(all)) {
    which_lines <-
      max(which_lines[0 > (which_lines - (as.numeric(context$selection[[1]]$range$start[1])))])
  }

  x <-
    tibble::data_frame(line = which_lines,
                      nline = 1) %>%
    dplyr::mutate(
      check_pipe = purrr::pmap_lgl(., ~ context$contents[c(..1)] %>%
                                     purrr::map_lgl(
                                       ~ !grepl("(design_|transcribe)\\(.+\\|(TRUE|FALSE))|(%>%|%<>%)",
                                                x = ..1)))) %>%
    dplyr::mutate(nline = purrr::pmap_int(.,
                                          ~ dplyr::if_else(rlang::is_true(..3),
                                                           2L,
                                                           1L)))

  out_lines <- unique(c(x$line, x$line - (x$nline - 1)))

  purrr::walk(out_lines, ~ rstudioapi::insertText(c(..1, 1), "# ", id = context$id))
  # nocov end
}

#' @noRd
put_all <- function() {
  put(all = TRUE)
}
