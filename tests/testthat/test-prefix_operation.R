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

test_that("substract prefix works", {
  expect.str <- "columnContainingData"
  result.str <- substract_prefix(str, "B_")
  expect_equal(expect.str, result.str)
})

A_Column1 <- c(1, 2)
A_Column2 <- c(3, 4)
A_Column3 <- c(5, 6)

Column1 <- c(1, 2)
Column2 <- c(3, 4)
Column3 <- c(5, 6)

test_that("prefix vector works with a special prefix separator", {
  df <- data.frame(A_Column1, A_Column2, A_Column3)
  expect.df <- data.frame(Column1, Column2, Column3)
  result.df <- substract_prefix_from_all_column(df, "A_")
  expect_equal(expect.df, result.df)
})


A_column <- c(1, 2, 3)
B_column <- c(4, 5, 6)
C_column <- c(7, 8, 9)

test_that("prefix vector works with a special prefix separator", {
  df <- data.frame(A_column, B_column, C_column)
  expect.vec <- c("A", "B", "C")
  result.vec <- prefix_vector(df, "_")
  expect_equal(expect.vec, result.vec)
})
