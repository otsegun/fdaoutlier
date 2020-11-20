test_that("tvd_mss finds outliers produces correct results", {
  dt <- sim_data1$data[1:9, 1:7]
  tvdmssobj <- tvd_mss(dt)

  expect_null(tvdmssobj$shape_outliers)
  expect_equal(tvdmssobj$magnitude_outliers, 4)
  expect_equal(tvdmssobj$outliers, 4)


})

test_that("tvd_mss behaves well when outliers are not found", {
  dt <- sim_data1$data[c(1:3, 5:9), 1:7]
  tvdmssobj <- tvd_mss(dt)

  expect_null(tvdmssobj$shape_outliers)
  expect_null(tvdmssobj$magnitude_outliers)

})

test_that("tvd_mss behaves well when shape outliers are found", {
  add_shape <- rbind(sim_data1$data, rep(c(-5, 5), 25))
  tvdmssobj <- tvd_mss(add_shape)
  expect_equal(tvdmssobj$shape_outliers, 101)

})
