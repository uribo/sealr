context("test-sf.R")

test_that("integration tst", {

  df_sf <- structure(
    list(structure(
      list(structure(
        c(
          139.7125,
          139.725,
          139.725,
          139.7125,
          139.7125,
          35.7,
          35.7,
          35.70833,
          35.70833,
          35.7
        ),
        .Dim = c(5L, 2L)
      )), class = c("XY", "POLYGON", "sfg")
    )),
    n_empty = 0L,
    crs = structure(
      list(epsg = 4326L, proj4string = "+proj=longlat +datum=WGS84 +no_defs"),
      .Names = c("epsg",
                 "proj4string"),
      class = "crs"
    ),
    class = c("sfc_POLYGON", "sfc"),
    precision = 0,
    bbox = structure(
      c(139.7125, 35.7, 139.725,
        35.70833),
      .Names = c("xmin", "ymin", "xmax", "ymax"),
      class = "bbox"
    )
  )

  sf_point <- structure(c(0, 0), class = c("XY", "POINT", "sfg"))

  e <- new.env()
  assign("df_sf", df_sf, e)
  assign("sf_point", sf_point, e)
  withr::with_environment(
    e, {
      expect_equal(
        design_geom_type(sf_point),
        "tst::expect_geom_type(\nsf_point,\n\"POINT\"\n)"
      )
      expect_equal(
        design_crs(df_sf),
        "tst::expect_crs(\nsf::st_crs(df_sf)[1]$epsg,\n4326L\n)"
      )
      expect_equal(
        design_sf_dimension(df_sf),
        "tst::expect_dimension(\ndf_sf,\n2L\n)"
      )
    }
  )
})
