#' Print an \R{} sf object test result
#'
#' @inheritParams design
#' @inheritDotParams seal -test
#' @name design-sf
NULL

#' @rdname design-sf
#' @examples
#' \dontrun{
#' library(sf)
#' sf_point <- st_point(c(0, 0))
#' sf_line <- st_linestring(matrix(1:12, ncol = 3))
#'
#' design_geom_type(sf_point)
#' design_geom_type(sf_line)
#' }
#' @export
design_geom_type <- function(x, ...) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue("tst::expect_geom_type(
               {x},
               \n",
               x = get("obj", e)),
    rlang::expr_text(as.character(sf::st_geometry_type(x))),
    "\n)"
  )) %>%
    sealing(...)
}

#' @rdname design-sf
#' @examples
#' \dontrun{
#' library(sf)
#' nc <-
#'   system.file("shape/nc.shp", package = "sf") %>%
#'   st_read()
#' design_crs(nc)
#' }
#' @export
design_crs <- function(x, ...) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue("tst::expect_crs(
               sf::st_crs({x})[1]$epsg,
               \n",
               x = get("obj", e)),
    rlang::expr_text(sf::st_crs(x)[1]$epsg),
    "\n)"
  )) %>%
    sealing(...)
}

#' @rdname design-sf
#' @examples
#' \dontrun{
#' library(sf)
#' sf_point <- st_point(c(0, 0))
#'
#' design_sf_dimension(sf_point, 0)
#'
#' # Failed
#' design_sf_dimension(sf_point, 1)
#' }
#' @export
design_sf_dimension <- function(x, ...) {
  e <- compound(x)

  as.character(glue::glue(
    glue::glue("tst::expect_dimension(
               {x},
               \n",
               x = get("obj", e)),
    rlang::expr_text(sf::st_dimension(x)),
    "\n)"
  )) %>%
    sealing(...)
}
