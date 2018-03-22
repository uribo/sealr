context("test-design.R")

test_that("control transcript behavior functions", {

  expect_equal(
    design_nrow(iris),
    "expect_equal(\nnrow(iris),\n150L\n)"
  )
  expect_equal(
    design_ncol(mtcars),
    "expect_equal(\nncol(mtcars),\n11L\n)"
  )

  # Fixed #2
  expect_length(
      design_length(x = c(letters, LETTERS)),
      1L
  )

  expect_equal(
    design_unique(c("a", "a", "b")),
    "expect_equal(\nunique(x),\nc(\"a\", \"b\")\n)"
  )

  expect_equal(
    design_missings(x = c(1, NA, 3)),
    "expect_equal(\nsum(is.na(x)),\n1L\n)"
  )

  expect_equal(
    design_class(iris),
    "expect_is(\niris,\n\"data.frame\"\n)"
  )

  skip_if_not_installed("dplyr")
  withr::with_package(
    "dplyr", {
      expect_equal(
        design_class(band_members),
        "expect_is(\nband_members,\nc(\"tbl_df\", \"tbl\", \"data.frame\")\n)"
      )
    }
  )

  expect_message(
    expect_equal(
      design_dimnames(
        x = matrix(1:10,
                   nrow = 1,
                   ncol = 2,
                   dimnames = list("row", c("col1", "col2")))),
      # nolint start
      "expect_equal(\ndimnames(x),\nlist(\"row\", c(\"col1\", \"col2\"))\n)"
      # nolint end
    )
  )

  expect_equal(
    design_obj_size(iris),
    "expect_equal(\nlobstr::obj_size(iris),\nstructure(7032, class = \"lobstr_bytes\")\n)" # nolint
  )

  e <- new.env()
  my_data <- data.frame(A = letters)
  assign("my_data", my_data, e)
  withr::with_environment(
    e, {
      expect_equal(
        design_obj_size(my_data),
        "expect_equal(\nlobstr::obj_size(my_data),\nstructure(2560, class = \"lobstr_bytes\")\n)"
      )
    }
  )

  iris_s4 <- asS4(iris)
  assign("iris_s4", iris_s4, e)
  withr::with_environment(
    e, {
      expect_equal(
        design_class(iris_s4, environment = e),
        "expect_s4_class(\niris_s4,\n\"data.frame\"\n)"
      )
    }
  )
})
