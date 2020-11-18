# test_that("mbd handles strange data", {
#   expect_error(modified_band_depth(dt = list()))
#   expect_error(modified_band_depth(dt = data.frame()) )
#   expect_error(modified_band_depth(dt = matrix(NA, nrow = 1, ncol = 3)))
#   expect_error(modified_band_depth(dt = matrix(c(1:2, NA), nrow = 1, ncol = 3)))
# })
#
#
# test_that("mbd gives correct result", {
#   mbdd <- modified_band_depth(sim_data1$data)
#   expect_equal(order(mbdd)[1:10],
#                c(79, 82,  4, 23, 84, 17, 59, 40, 12, 83))
#   expect_equal(order(mbdd)[90:100],
#                c(28, 77, 72, 15, 10, 71, 73, 33, 99, 16, 32))
# })
