context("test-transcribe.R")

test_that("methods", {

  x <- c(1, 3)
  expect_equal(
    transcribe(x),
    # nolint start
    "test_that(\"x\", {expect_is(\nx,\n\"numeric\")\nexpect_length(\nx ,\n 2L)\nexpect_length(\nunique(x),\n2L)\nexpect_equal(\nrange( x ),\n c(1, 3))})"
    # nolint end
  )
  expect_is(
    class(transcribe(letters)),
    "character"
  )
  x <- iris$Species

  expect_equal(
    capture_output({
      transcribe(x) %>% seal(clip = FALSE)
    },
    print = TRUE, width = 80),
    # nolint start
    "test_that(\"x\", {\n  expect_is(\n    x,\n    \"factor\"\n  )\n  expect_length(\n    x,\n    150L\n  )\n  expect_equal(\n    levels(x),\n    c(\"setosa\", \"versicolor\", \"virginica\")\n  )\n  expect_equal(\n    nlevels(x),\n    3L\n  )\n})"
    # nolint end
  )

  expect_equal(
    transcribe(iris),
    # nolint start
    "test_that(\"iris\", {expect_is(\niris,\n\"data.frame\")\nexpect_equal(\ndim( iris ),\nc(150L, 5L))\nexpect_named(\niris,\nc(\"Sepal.Length\", \"Sepal.Width\", \"Petal.Length\", \"Petal.Width\", \n\"Species\"))\nexpect_equal(\niris%>% purrr::map_chr(class) %>% unname(),\nc(\"numeric\", \"numeric\", \"numeric\", \"numeric\", \"factor\"))})"
    # nolint end
  )


})
