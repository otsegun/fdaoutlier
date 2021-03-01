dt1 <- simulation_model1(seed = 50)
test_that("linfinity handles strange data", {
  expect_error(linfinity_depth(dt = list()))
  expect_error(linfinity_depth(dt = data.frame()) )
  expect_error(linfinity_depth(dt = matrix(NA, nrow = 1, ncol = 3)))
  expect_error(linfinity_depth(dt = matrix(c(1:2, NA), nrow = 1, ncol = 3)))
})

test_that("linfinity_depth gives correct result", {
  linda <- linfinity_depth(dt1$data)
  expect_equal(order(linda)[1:10],
               c(43, 53, 70, 20, 96, 36, 14,  3, 18, 16))
  expect_equal(order(linda)[90:100],
               c(24, 74, 87, 25, 34, 83, 80, 76, 23, 90, 47))

})

