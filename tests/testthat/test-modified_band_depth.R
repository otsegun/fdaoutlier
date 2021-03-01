dt1 <- simulation_model1(seed = 50)
test_that("mbd handles strange data", {
  expect_error(modified_band_depth(dt = list()))
  expect_error(modified_band_depth(dt = data.frame()) )
  expect_error(modified_band_depth(dt = matrix(NA, nrow = 1, ncol = 3)))
  expect_error(modified_band_depth(dt = matrix(c(1:2, NA), nrow = 1, ncol = 3)))
})


test_that("mbd gives correct result", {
  mbdd <- modified_band_depth(dt1$data)
  expect_equal(order(mbdd)[1:10],
               c(43, 70, 53, 36, 20, 14, 96,  3, 88, 58))
  expect_equal(order(mbdd)[90:100],
               c(80, 23, 87, 59, 11, 34, 54,  1, 25, 83, 47))
})
