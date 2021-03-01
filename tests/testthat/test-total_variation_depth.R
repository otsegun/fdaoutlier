test_that("total variation depth handles unconventional data", {
  expect_error(total_variation_depth(data = list()) )
  expect_error(total_variation_depth(data = data.frame()) )
  expect_error(total_variation_depth(data = matrix(0, nrow = 2, ncol = 3)))
})
dt1 <- simulation_model1(seed = 50)

test_that("total variation depth produces correct results", {
  dt <- dt1$data[1:9, 1:7]
  tvdobj <- total_variation_depth(dt)

  # tvd
  expect_equal(tvdobj$tvd, c(0.215167548500882, 0.232804232804233, 0.141093474426808,
                             0.130511463844797, 0.098765432098765, 0.236331569664903,
                             0.179894179894180, 0.000000000000000, 0.246913580246914))

  #mss
  expect_equal(tvdobj$mss, c(1.224865878152715, 1.044790186943456, 0.875771897170949,
                                   0.803291463139334, 1.206605323423426, 0.782621764558598,
                                   1.378874483912276, 1.337193243681630, 1.056225504077608))

})



