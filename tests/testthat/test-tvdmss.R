dt1 <- simulation_model1(seed = 50)
test_that("tvd_mss finds outliers produces correct results", {
  dt <- dt1$data[1:9, 1:7]
  expect_warning(tvdmssobj <- tvd_mss(dt))
  expect_null(tvdmssobj$shape_outliers)
  expect_equal(tvdmssobj$magnitude_outliers, c(5,8))
  expect_equal(tvdmssobj$outliers, c(5,8))



})

test_that("tvd_mss behaves well when outliers are not found", {
  dt <- dt1$data[c(1:3, 5:9), 1:7]
  expect_warning(tvdmssobj <- tvd_mss(dt))
  expect_null(tvdmssobj$shape_outliers)
})

test_that("tvd_mss behaves well when shape outliers are found", {
  dt6 <- simulation_model6(seed = 50)

  expect_warning(tvdmssobj <- tvd_mss(dt6$data))
  expect_equal(tvdmssobj$shape_outliers, c(14, 20, 43, 53, 96))

})
