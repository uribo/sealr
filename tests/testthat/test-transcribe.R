context("test-transcribe.R")

test_that("methods", {

  e <- new.env()
  x <- c(1, 3)
  my_species <- iris$Species
  assign("x", x, e)
  assign("my_species", my_species, e)
  withr::with_environment(
    e, {
      expect_equal(
        transcribe(x),
        # nolint start
        "test_that(\"x\", {expect_is(\nx,\n\"numeric\"\n)\nexpect_length(\nx,\n2L\n)\nexpect_equal(\nunique(x),\nc(1, 3)\n)\nexpect_equal(\nrange(x),\nc(1, 3)\n)})"
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
        "test_that(\"x\", {expect_is(\nx,\n\"numeric\"\n)\nexpect_length(\nx,\n2L\n)\nexpect_equal(\nunique(x),\nc(1, 3)\n)\nexpect_equal(\nrange(x),\nc(1, 3)\n)})"
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
