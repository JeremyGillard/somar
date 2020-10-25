context("prefix_operation")

str <- "B_columnContainingData"

test_that("extract prefix works with a normal character", {
  expect.str <- "B_column"
  result.str <- extract_prefix(str, "C")
  expect_equal(expect.str, result.str)
})

test_that("extract prefix works with a normal string", {
  expect.str <- "B_column"
  result.str <- extract_prefix(str, "Containing")
  expect_equal(expect.str, result.str)
})

test_that("extract prefix works with a special string", {
  expect.str <- "B"
  result.str <- extract_prefix(str, "_")
  expect_equal(expect.str, result.str)
})
