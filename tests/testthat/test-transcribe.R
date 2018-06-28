context("test-transcribe.R")

test_that("methods", {

  e <- new.env()
  x <- c(1, 3)
  my_species <- iris$Species
  date_obj <- Sys.Date()
  list_obj <- list(a = "A", b = 3, list(letters[1:3]))
  matrix_obj <-
    matrix(1:10, nrow = 1, ncol = 2, dimnames = list("row", c("col1", "col2")))
  table_obj <-
    table(
      c("A", "B"),
      c(0, 1)
    )

  assign("x", x, e)
  assign("my_species", my_species, e)
  assign("date_obj", date_obj, e)
  assign("list_obj", list_obj, e)
  assign("matrix_obj", matrix_obj, e)
  assign("table_obj", table_obj, e)

  withr::with_environment(
    e, {
      expect_equal(
        transcribe(date_obj, desc = NULL, seal = FALSE),
        "test_that(\"check date_obj statement\", {expect_is(\ndate_obj,\n\"Date\"\n)})"
      )
      expect_equal(
        transcribe(x, seal = FALSE),
        # nolint start
        "test_that(\"check x statement\", {expect_is(\nx,\n\"numeric\"\n)\nexpect_length(\nx,\n2L\n)\nexpect_equal(\nunique(x),\nc(1, 3)\n)\nexpect_equal(\nrange(x, na.rm = TRUE),\nc(1, 3)\n)})"
        # nolint end
      )
      expect_equal(
        transcribe(list_obj, seal = FALSE),
        # nolint start
        "test_that(\"check list_obj statement\", {expect_is(\nlist_obj,\n\"list\"\n)\nexpect_length(\nlist_obj,\n3L\n)\nexpect_named(\nlist_obj,\nc(\"a\", \"b\", \"\")\n)})"
        # nolint end
      )
      expect_equal(
        transcribe(matrix_obj, seal = FALSE),
        # nolint start
        "test_that(\"check matrix_obj statement\", {expect_is(\nmatrix_obj,\n\"matrix\"\n)\nexpect_equal(\ndim(matrix_obj),\n1:2\n)\nexpect_equal(\ndimnames(matrix_obj),\nlist(\"row\", c(\"col1\", \"col2\"))\n)})"
        # nolint end
      )
      expect_equal(
        capture_output({
          transcribe(my_species, desc = "Prove the state of the Species", seal = TRUE, clip = FALSE, ts = FALSE)
        },
        print = TRUE, width = 80),
        # nolint start
        "test_that(\"Prove the state of the Species\", {\n  expect_is(\n    my_species,\n    \"factor\"\n  )\n  expect_length(\n    my_species,\n    150L\n  )\n  expect_equal(\n    levels(my_species),\n    c(\"setosa\", \"versicolor\", \"virginica\")\n  )\n  expect_equal(\n    nlevels(my_species),\n    3L\n  )\n})"
        # nolint end
      )
    })

  skip_on_travis()
  skip_on_cran()
  withr::with_environment(
    e, {
      expect_equal(
        transcribe(table_obj, seal = FALSE),
        # nolint start
        "test_that(\"check table_obj statement\", {expect_is(\ntable_obj,\n\"table\"\n)\nexpect_equal(\ndim(table_obj),\nc(2L, 2L)\n)\nexpect_equal(\ndimnames(table_obj),\nstructure(list(c(\"A\", \"B\"), c(\"0\", \"1\")), .Names = c(\"\", \"\"))\n)})"
        # nolint end
      )
    })

  expect_is(
    class(transcribe(letters, seal = FALSE)),
    "character"
  )

  # FIXME
  expect_error(
    expect_message(
      c(1, 3) %>% transcribe()
    ))
  expect_error(
    expect_message(
      data.frame(a = 1) %>% transcribe()
    ))

  expect_equal(
    transcribe(letters, seal = FALSE),
    letters %>% transcribe(seal = FALSE)
  )
  expect_equal(
    transcribe(iris, seal = FALSE),
    iris %>% transcribe(seal = FALSE)
  )

  e <- new.env()
  x <- c(1, 3)
  assign("x", x, e)
  withr::with_environment(
    e, {
      expect_equal(
        x %>% transcribe(seal = FALSE),
        # nolint start
        "test_that(\"check x statement\", {expect_is(\nx,\n\"numeric\"\n)\nexpect_length(\nx,\n2L\n)\nexpect_equal(\nunique(x),\nc(1, 3)\n)\nexpect_equal(\nrange(x, na.rm = TRUE),\nc(1, 3)\n)})"
        # nolint end
      )
    })

  expect_equal(
    transcribe(iris, seal = FALSE),
    # nolint start
    "test_that(\"check iris statement\", {expect_is(\niris,\n\"data.frame\"\n)\nexpect_equal(\ndim(iris),\nc(150L, 5L)\n)\nexpect_named(\niris,\nc(\"Sepal.Length\", \"Sepal.Width\", \"Petal.Length\", \"Petal.Width\", \n\"Species\")\n)\nexpect_equal(\niris %>% purrr::map(class) %>% unname(),\nlist(\"numeric\", \"numeric\", \"numeric\", \"numeric\", \"factor\")\n)})"
    # nolint end
  )

  skip_if_not_installed("dplyr")
  skip_on_cran()
  withr::with_package(
    "dplyr", {
      expect_equal(
        transcribe(band_members, seal = FALSE),
        # nolint start
        "test_that(\"check band_members statement\", {expect_is(\nband_members,\nc(\"tbl_df\", \"tbl\", \"data.frame\")\n)\nexpect_equal(\ndim(band_members),\n3:2\n)\nexpect_named(\nband_members,\nc(\"name\", \"band\")\n)\nexpect_equal(\nband_members %>% purrr::map(class) %>% unname(),\nlist(\"character\", \"character\")\n)})"
        # nolint end
      )
    }
  )

})

test_that("Operator Access", {
  expect_equal(
    .design_df_details("iris$Species"),
    "expect_equal(\n    sum(is.na(iris$Species)),\n0L)\nexpect_equal(\n      iris$Species,\nc(\"setosa\", \"versicolor\", \"virginica\"))" # nolint
  )
  expect_identical(
    "mtcars$mpg" %>% .design_df_details(),
    .design_df_details("mtcars$mpg")
  )
  expect_error(
    .design_df_details(iris$Species)
  )
  .design_df_details("letters")

  e <- new.env()
  df <- data.frame(x = c(Sys.Date(), NA))
  assign("df", df, e)
  withr::with_environment(
    e, {
      expect_equal(
        .design_df_details("df$x"),
        "expect_equal(\n    sum(is.na(df$x)),\n1L)\nexpect_equal(\n      df$x,\n2L)"
      )
    })
})

test_that("Test for data frame variables", {
  e <- new.env()
  df <- data.frame(x = letters)
  assign("df", df, e)
  withr::with_environment(
    e, {
      expect_equal(
        transcribe(df, desc = "for test", seal = FALSE, clip = FALSE, ts = FALSE, detail = TRUE),
        "test_that(\"for test\", {expect_is(\ndf,\n\"data.frame\"\n)\nexpect_equal(\ndim(df),\nc(26L, 1L)\n)\nexpect_named(\ndf,\n\"x\"\n)\nexpect_equal(\ndf %>% purrr::map(class) %>% unname(),\nlist(\"factor\")\n)\nexpect_equal(\n    sum(is.na(df$x)),\n0L)\nexpect_equal(\n      df$x,\nc(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\", \"i\", \"j\", \"k\", \"l\", \n\"m\", \"n\", \"o\", \"p\", \"q\", \"r\", \"s\", \"t\", \"u\", \"v\", \"w\", \"x\", \"y\", \n\"z\"))})" # nolint
      )})
  expect_equal(
    capture_output({
      transcribe(iris, seal = TRUE, clip = FALSE, ts = FALSE, detail = TRUE)
    },
    print = TRUE, width = 80),
    # nolint start
    "test_that(\"check iris statement\", {\n  expect_is(\n    iris,\n    \"data.frame\"\n  )\n  expect_equal(\n    dim(iris),\n    c(150L, 5L)\n  )\n  expect_named(\n    iris,\n    c(\n      \"Sepal.Length\", \"Sepal.Width\", \"Petal.Length\", \"Petal.Width\",\n      \"Species\"\n    )\n  )\n  expect_equal(\n    iris %>% purrr::map(class) %>% unname(),\n    list(\"numeric\", \"numeric\", \"numeric\", \"numeric\", \"factor\")\n  )\n  expect_equal(\n    sum(is.na(iris$Petal.Length)),\n    0L\n  )\n  expect_equal(\n    range(iris$Petal.Length, na.rm = TRUE),\n    c(1, 6.9)\n  )\n  expect_equal(\n    sum(is.na(iris$Petal.Width)),\n    0L\n  )\n  expect_equal(\n    range(iris$Petal.Width, na.rm = TRUE),\n    c(0.1, 2.5)\n  )\n  expect_equal(\n    sum(is.na(iris$Sepal.Length)),\n    0L\n  )\n  expect_equal(\n    range(iris$Sepal.Length, na.rm = TRUE),\n    c(4.3, 7.9)\n  )\n  expect_equal(\n    sum(is.na(iris$Sepal.Width)),\n    0L\n  )\n  expect_equal(\n    range(iris$Sepal.Width, na.rm = TRUE),\n    c(2, 4.4)\n  )\n  expect_equal(\n    sum(is.na(iris$Species)),\n    0L\n  )\n  expect_equal(\n    iris$Species,\n    c(\"setosa\", \"versicolor\", \"virginica\")\n  )\n})"
    # nolint end
  )
})
