#' catch up data.frame variable names and value
#'
#' @inheritParams design
#' @param variable target data.frame's column name (`character`)
#'
#' @examples
#' \dontrun{
#' # Storage R object to a temporary environment
#' store_to_env(iris)
#'
#' catch_varnames(mtcars)
#'
#' # Evaluate
#' library(glue)
#' evaluate(catch_varname(mtcars, "mpg"), .GlobalEnv)
#' }
#' @name catch_vars
NULL

#' @rdname catch_vars
store_to_env <- function(x) {
  e <- compound(x)
  vars <- paste(get("obj", e), names(x), sep = "$")

  purrr::walk(1:length(vars), ~ assign(vars[.x], x[, .x], envir = e))
  rm("obj", envir = e)
  e
}

#' @rdname catch_vars
catch_varnames <- function(x) {
  obj <- store_to_env(x)
  names(obj)
}

#' @rdname catch_vars
catch_varname <- function(x, variable) {
  if (length(variable) != 1) {
    rlang::abort(
      "Enable a single variable name.\nIf you want collect multiple variable names to use catch_df_varames()"  # nolint
    )
  }

  catch_varnames(x) %>%
    grep(paste0("\\$",
                rlang::eval_tidy(rlang::quo(!!variable)),
                "$"), ., value = TRUE)
}
