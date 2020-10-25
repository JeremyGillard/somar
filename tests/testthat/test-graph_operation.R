context("graph_operation")

SelIndex_SelThumb <- c(1, 2)
SelIndex_SelPinky <- c(3, 4)

test_that("node vector from dataframe unit works correctly", {
  df <- data.frame(SelIndex_SelThumb, SelIndex_SelPinky)
  expect.vec <- c("SelIndex", "SelThumb", "SelIndex", "SelPinky")
  result.vec <- node_vector_from_dataframe_unit(df)
  expect_equal(expect.vec, result.vec)
})
