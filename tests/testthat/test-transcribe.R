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
        transcribe(date_obj),
        "test_that(\"date_obj\", {expect_is(\ndate_obj,\n\"Date\"\n)})"
      )
      expect_equal(
        transcribe(x),
        # nolint start
        "test_that(\"x\", {expect_is(\nx,\n\"numeric\"\n)\nexpect_length(\nx,\n2L\n)\nexpect_equal(\nunique(x),\nc(1, 3)\n)\nexpect_equal(\nrange(x, na.rm = TRUE),\nc(1, 3)\n)})"
        # nolint end
      )
      expect_equal(
        transcribe(list_obj),
        # nolint start
        "test_that(\"list_obj\", {expect_is(\nlist_obj,\n\"list\"\n)\nexpect_length(\nlist_obj,\n3L\n)\nexpect_named(\nlist_obj,\nc(\"a\", \"b\", \"\")\n)})"
        # nolint end
      )
      expect_equal(
        transcribe(matrix_obj),
        # nolint start
        "test_that(\"matrix_obj\", {expect_is(\nmatrix_obj,\n\"matrix\"\n)\nexpect_equal(\ndim(matrix_obj),\n1:2\n)\nexpect_equal(\ndimnames(matrix_obj),\nlist(\"row\", c(\"col1\", \"col2\"))\n)})"
        # nolint end
      )
      expect_equal(
        transcribe(table_obj),
        # nolint start
        "test_that(\"table_obj\", {expect_is(\ntable_obj,\n\"table\"\n)\nexpect_equal(\ndim(table_obj),\nc(2L, 2L)\n)\nexpect_equal(\ndimnames(table_obj),\nstructure(list(c(\"A\", \"B\"), c(\"0\", \"1\")), .Names = c(\"\", \"\"))\n)})"
        # nolint end
      )
      expect_equal(
        capture_output({
          transcribe(my_species) %>%
            seal(clip = FALSE)
        },
        print = TRUE, width = 80),
        # nolint start
        "test_that(\"my_species\", {\n  expect_is(\n    my_species,\n    \"factor\"\n  )\n  expect_length(\n    my_species,\n    150L\n  )\n  expect_equal(\n    levels(my_species),\n    c(\"setosa\", \"versicolor\", \"virginica\")\n  )\n  expect_equal(\n    nlevels(my_species),\n    3L\n  )\n})"
        # nolint end
      )
      })
  expect_is(
    class(transcribe(letters)),
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
    transcribe(letters),
    letters %>% transcribe()
  )
  expect_equal(
    transcribe(iris),
    iris %>% transcribe()
  )

  e <- new.env()
  x <- c(1, 3)
  assign("x", x, e)
  withr::with_environment(
    e, {
      expect_equal(
        x %>% transcribe(),
        # nolint start
        "test_that(\"x\", {expect_is(\nx,\n\"numeric\"\n)\nexpect_length(\nx,\n2L\n)\nexpect_equal(\nunique(x),\nc(1, 3)\n)\nexpect_equal(\nrange(x, na.rm = TRUE),\nc(1, 3)\n)})"
        # nolint end
      )
    })

  expect_equal(
    transcribe(iris),
    # nolint start
    "test_that(\"iris\", {expect_is(\niris,\n\"data.frame\"\n)\nexpect_equal(\ndim(iris),\nc(150L, 5L)\n)\nexpect_named(\niris,\nc(\"Sepal.Length\", \"Sepal.Width\", \"Petal.Length\", \"Petal.Width\", \n\"Species\")\n)\nexpect_equal(\niris %>% purrr::map_chr(class) %>% unname(),\nc(\"numeric\", \"numeric\", \"numeric\", \"numeric\", \"factor\")\n)})"
    # nolint end
  )


})
