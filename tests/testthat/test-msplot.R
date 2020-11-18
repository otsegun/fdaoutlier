# test_that("msplot works", {
#   multiv_data <- c(-1.00699287, -0.53823436, -0.55879513, -0.18606117, -0.52050693,
#                    0.46096140, -0.32240037, -0.46775918, -0.38068162, 0.02840495,
#                    -0.16316400,  0.18996883, -0.25430960, -0.06389951, -0.46119745,
#                    -0.30353525, -1.21554894, -1.22182643, -0.63857941, -0.84320227,
#                    -0.77277610, -0.88696090, -0.80527829, -0.33013994, -0.25724301,
#                    0.86073945, -0.75379286, -0.81968759, -0.57461555, -1.02516021)
#
#   multiv_data <- array(multiv_data, dim = c(5, 3, 2))
#   univ_data <- multiv_data[,,1]
#
#   ms_multiv <- msplot(data = multiv_data, n_projections = 7, seed = 20, return_mvdir = TRUE)
#   ms_univ <- msplot(data = univ_data, n_projections = 7, seed = 20, return_mvdir = TRUE)
#
#   #univariate mean of outlyingness
#   # tmp_msplot_univ <- "tests/reference_files/ref_msplot_meanout_univ.rda"
#   # expect_known_value(ms_univ$mean_outlyingness, tmp_msplot_univ, update = T, print = T)
#   expect_known_value(ms_univ$mean_outlyingness, "../reference_files/ref_msplot_meanout_univ.rda",
#                      update = F, print = T)
#
#   #multivariate variation of outlyingness
#   # tmp_msplot_multiv <- "tests/reference_files/ref_msplot_varout_multiv.rda"
#   # expect_known_value(ms_multiv$var_outlyingness, tmp_msplot_multiv, update = T, print = T)
#   expect_known_value(ms_multiv$var_outlyingness, "../reference_files/rref_msplot_varout_multiv.rda",
#                      update = F, print = T)
#
#
# })
