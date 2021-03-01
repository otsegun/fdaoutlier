test_that("directional quantile depth handles unconventional data", {
  expect_error(directional_quantile(dt = list()))
  expect_error(directional_quantile(dt = data.frame()) )
  expect_error(directional_quantile(dt = matrix(0, nrow = 1, ncol = 3)))
  expect_error(directional_quantile(dt = matrix(NA, nrow = 1, ncol = 3)))
  expect_error(directional_quantile(dt = matrix(c(1:2, NA), nrow = 1, ncol = 3)))
})

test_that("directional quantile filters quantile probabilites", {
  dt1 <- simulation_model1(seed = 50)
  expect_error(directional_quantile(dt = dt1$data, quantiles = c(-1, .5 )))
  expect_error(directional_quantile(dt = dt1$data, quantiles = c(.025, NA)))
  expect_error(directional_quantile(dt = dt1$data, quantiles = NA))
})

test_that("directional quantile is correct", {
  dt1 <- simulation_model1(seed = 50)
  dttt <- dt1$data[1:9, 1:7]
  dtt <- directional_quantile(dttt)
  # right
  expect_equal(order(dtt), c(1, 9, 7, 2, 5, 6, 4, 3, 8))
  expect_equal(dtt, c(0.273265020112457, 0.506686936102822, 1.099752667970248,
                      1.039569976781981, 0.750125646435782, 0.760621786914891,
                      0.484546683430236, 1.120309121534383, 0.411845430081462))
})
