#' Catch-up data frame variable names and value
#'
#' @param data A data frame.
#' @param variable target data frame's column name (`character`)
#'
#' @examples
#' \dontrun{
#' catch_varnames(mtcars)
#'
#' # Evaluate
#' library(glue)
#' evaluate(catch_varname(mtcars, "mpg"), .GlobalEnv)
#' }
#' @name catch_vars
NULL

#' @rdname catch_vars
catch_varnames <- function(data) {
  if (is.data.frame(data)) {
    paste(unique(lhs_name(get(
      "data", environment()
    ))), names(data), sep = "$")
  } else {
    rlang::abort("Only applied to an object of class \"data.frame\"")
  }
}

#' @rdname catch_vars
catch_varname <- function(data, variable) {
  if (length(variable) != 1) {
    rlang::abort(
      "Enable a single variable name.\nIf you want collect multiple variable names to use catch_df_varames()"  # nolint
    )
  }

  catch_varnames(data) %>%
    grep(paste0("\\$",
                rlang::eval_tidy(rlang::quo(!!variable)),
                "$"), ., value = TRUE)
}
