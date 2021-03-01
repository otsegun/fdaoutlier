test_that("bd handles strange data", {
  expect_error(band_depth(dt = list()),
               "Argument \"dt\" must be a nonempty numeric matrix or dataframe.")
  dta <- data.frame()
  expect_error(band_depth(dt = data.frame()))

  expect_error(band_depth(dt = matrix(NA, nrow = 1, ncol = 3)))
  expect_error(band_depth(dt = matrix(c(1:2, NA), nrow = 1, ncol = 3)))
})


test_that("bd gives correct result", {
  dt1 <- simulation_model1(seed = 50)
  bdd <- band_depth(dt1$data)
  expect_equal(order(bdd)[1:10],
               c(20, 43, 53, 70,  9,  6, 14, 36, 93,  8))
  expect_equal(order(bdd)[90:100],
               c(17, 54, 15, 34, 83, 74, 25, 24, 23, 47, 90))

})
