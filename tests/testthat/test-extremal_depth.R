test_that("extremal depth handles unconventional data", {
  expect_error(extremal_depth(dt = list()) )
  expect_error(extremal_depth(dt = data.frame()) )
  expect_error(extremal_depth(dt = matrix(0, nrow = 2, ncol = 3)))
})

test_that("extremal depth produces correct results", {
  dt1 <- simulation_model1(seed = 50)
  dt <- dt1$data[1:9, 1:7]
  ed <- extremal_depth(dt)

  # tvd
  # tmp_tvd_tvd <- "tests/reference_files/ref_tvd_tvd.rda"
  # expect_known_value(tvdobj$tvd, tmp_tvd_tvd, update = T, print = T)
  expect_equal(order(ed, decreasing = T), c(9, 1, 6, 2, 7, 5, 3, 4, 8))
})
