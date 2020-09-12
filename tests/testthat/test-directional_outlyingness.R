
test_that("dir_out handles unconventional inputs, non-matrices,  and non-array objects", {
  expect_error(dir_out(data = list()) )
  expect_error(dir_out(data = data.frame()) )
})


test_that("dir_out produces correct results", {
  multiv_data <- c(-1.00699287, -0.53823436, -0.55879513, -0.18606117, -0.52050693,
                   0.46096140, -0.32240037, -0.46775918, -0.38068162, 0.02840495,
                   -0.16316400,  0.18996883, -0.25430960, -0.06389951, -0.46119745,
                   -0.30353525, -1.21554894, -1.22182643, -0.63857941, -0.84320227,
                   -0.77277610, -0.88696090, -0.80527829, -0.33013994, -0.25724301,
                   0.86073945, -0.75379286, -0.81968759, -0.57461555, -1.02516021)

  multiv_data <- array(multiv_data, dim = c(5, 3, 2))
  univ_data <- multiv_data[,,1]

  multiv_result <- dir_out(data = multiv_data, return_dir_matrix = T, data_depth = "random_projections", return_distance = T)
  multiv_result2 <- dir_out(data = multiv_data, return_dir_matrix = T, data_depth = "random_projections2", return_distance = T)

  univ_result <- dir_out(data = univ_data, return_dir_matrix = T, data_depth = "random_projections", return_distance = T)

  # that randomprojection and randomprojection2 are the same
  expect_equal(multiv_result, multiv_result)

  # that a list a returned
  expect_is(multiv_result,  "list")
  expect_is(univ_result, "list")

  ## univariate correctness
  # distance
  tmp_distance_univ <- tempfile()
  expect_known_value(univ_result$distance, tmp_distance_univ, print = T)
  # directional outlyingness matrix
  tmp_dirout_univ <- tempfile()
  expect_known_value(univ_result$dirout_matrix, tmp_dirout_univ, print = T)
  # mean outlyingness
  tmp_mean_out_univ <- tempfile()
  expect_known_value(univ_result$mean_outlyingness, tmp_mean_out_univ, print = T)
  # var outlyingness
  tmp_var_out_univ <- tempfile()
  expect_known_value(univ_result$var_outlyingness, tmp_var_out_univ, print = T)
  # centre
  tmp_centre_univ <- tempfile()
  expect_known_value(univ_result$center, tmp_centre_univ, print = T)

  ## multivariate correcness
  # that distance is correct
  tmp_distance <- tempfile()
  expect_known_value(multiv_result$distance, tmp_distance, print = T)
  # directional outlyingness matrix
  tmp_dirout <- tempfile()
  expect_known_value(multiv_result$dirout_matrix, tmp_dirout, print =T)
  # mean outlyingness
  tmp_mean_out <- tempfile()
  expect_known_value(multiv_result$mean_outlyingness, tmp_mean_out, print = T)
  # var outlyingness
  tmp_var_out <- tempfile()
  expect_known_value(multiv_result$var_outlyingness, tmp_var_out, print = T)
  # centre
  tmp_centre <- tempfile()
  expect_known_value(multiv_result$center, tmp_centre, print = T)
})







