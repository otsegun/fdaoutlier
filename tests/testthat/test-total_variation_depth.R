test_that("total variation depth handles unconventional data", {
  expect_error(total_variation_depth(data = list()) )
  expect_error(total_variation_depth(data = data.frame()) )
  expect_error(total_variation_depth(data = matrix(0, nrow = 2, ncol = 3)))
})

test_that("total variation depth produces correct results", {
  dt <- sim_data1$data[1:9, 1:7]
  tvdobj <- total_variation_depth(dt)

  # tvd
  # tmp_tvd_tvd <- "tests/reference_files/ref_tvd_tvd.rda"
  # expect_known_value(tvdobj$tvd, tmp_tvd_tvd, update = T, print = T)
  expect_known_value(tvdobj$tvd, "../reference_files/ref_tvd_tvd.rda",
                     update = F, print = T)

  #mss
  # tmp_tvd_mss <- "tests/reference_files/ref_tvd_mss.rda"
  # expect_known_value(tvdobj$mss, tmp_tvd_mss, update = T, print = T)
  expect_known_value(tvdobj$mss, "../reference_files/ref_tvd_mss.rda",
                     update = F, print = T)

})



