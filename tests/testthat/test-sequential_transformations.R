test_that("directional quantile depth handles unconventional data", {
  expect_error(seq_transform(dt = list()))
  expect_error(seq_transform(dt = data.frame()) )
  expect_error(seq_transform(dt = matrix(0, nrow = 1, ncol = 3)))
  expect_error(seq_transform(dt = matrix(NA, nrow = 1, ncol = 3)))
  expect_error(seq_transform(dt = matrix(c(1:2, NA), nrow = 1, ncol = 3)))
})
