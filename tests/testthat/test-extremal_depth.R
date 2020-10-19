test_that("extremal depth handles unconventional data", {
  expect_error(total_variation_depth(data = list()) )
  expect_error(total_variation_depth(data = data.frame()) )
  expect_error(total_variation_depth(data = matrix(0, nrow = 2, ncol = 3)))
})

test_that("extremal depth produces correct results", {
  dt <- sim_data1$data[1:9, 1:7]
  ed <- extremal_depth(dt)

  # tvd
  # tmp_tvd_tvd <- "tests/reference_files/ref_tvd_tvd.rda"
  # expect_known_value(tvdobj$tvd, tmp_tvd_tvd, update = T, print = T)
  expect_equal(order(ed, decreasing = T), c(2, 1, 6, 9, 8, 5, 3, 7, 4))
})
