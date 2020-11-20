
test_that("linfinity handles strange data", {
  expect_error(linfinity_depth(dt = list()))
  expect_error(linfinity_depth(dt = data.frame()) )
  expect_error(linfinity_depth(dt = matrix(NA, nrow = 1, ncol = 3)))
  expect_error(linfinity_depth(dt = matrix(c(1:2, NA), nrow = 1, ncol = 3)))
})


test_that("linfinity_depth gives correct result", {
  linda <- linfinity_depth(sim_data1$data)
  expect_equal(order(linda)[1:10],
               c(79, 23, 82, 84,  4, 17, 59, 40, 12, 83))
  expect_equal(order(linda)[90:100],
               c(16, 69, 71, 32, 22, 10, 67, 73, 99, 41, 27))

})

