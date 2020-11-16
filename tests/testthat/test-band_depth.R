test_that("bd handles strange data", {
  expect_error(band_depth(dt = list()))
  expect_error(band_depth(dt = data.frame()) )
  expect_error(band_depth(dt = matrix(NA, nrow = 1, ncol = 3)))
  expect_error(band_depth(dt = matrix(c(1:2, NA), nrow = 1, ncol = 3)))
})


test_that("bd gives correct result", {
  bdd <- band_depth(sim_data1$data)
  expect_equal(order(bdd)[1:10],
               c(4, 17, 79, 82, 70, 12, 23, 84, 30, 87))
  expect_equal(order(bdd)[90:100],
               c(98, 72, 45, 41, 80, 71, 33, 99, 32, 67, 10))

})
