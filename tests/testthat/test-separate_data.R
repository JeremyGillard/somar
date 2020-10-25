context("separate_data")

A_column <- c(1, 2, 3)
B_column <- c(4, 5, 6)
C_column <- c(7, 8, 9)

test_that("separate data normal mode", {
  df <- data.frame(A_column, B_column, C_column)
  expect.df <- data.frame(B_column)
  result.df <- separate_data(df, "^B")
  expect_identical(expect.df, result.df)
})

test_that("separate data reverse mode", {
  df <- data.frame(A_column, B_column, C_column)
  expect.df <- data.frame(A_column, C_column)
  result.df <- separate_data(df, "^B", inverse = TRUE)
  expect_identical(expect.df, result.df)
})

test_that("separate data with no pattern", {
  df <- data.frame(A_column, B_column, C_column)
  expect.df <- df
  result.df <- separate_data(df, "")
  expect_identical(expect.df, result.df)
})
