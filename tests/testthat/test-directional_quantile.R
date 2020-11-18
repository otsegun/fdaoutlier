# test_that("directional quantile depth handles unconventional data", {
#   expect_error(directional_quantile(dt = list()))
#   expect_error(directional_quantile(dt = data.frame()) )
#   expect_error(directional_quantile(dt = matrix(0, nrow = 1, ncol = 3)))
#   expect_error(directional_quantile(dt = matrix(NA, nrow = 1, ncol = 3)))
#   expect_error(directional_quantile(dt = matrix(c(1:2, NA), nrow = 1, ncol = 3)))
# })

# test_that("directional quantile filters quantile probabilites", {
#   expect_error(directional_quantile(dt = sim_data1$data, quantiles = c(-1, .5 )))
#   expect_error(directional_quantile(dt = sim_data1$data, quantiles = c(.025, NA)))
#   expect_error(directional_quantile(dt = sim_data1$data, quantiles = NA))
# })
#
#
# test_that("directional quantile is correct", {
#   dt <- sim_data1$data[1:9, 1:7]
#   dtt <- directional_quantile(dt)
#
#   # right
#   expect_equal(order(dtt), c(8, 9, 6, 1, 2, 3, 5, 7, 4))
#
#   expect_equal(dtt, c(0.851392557536830, 0.945160701035512, 1.009503146824852,
#                       1.2432485, 1.011295391788663, 0.580261283157357, 1.054868162713239,
#                       0.113764165115427, 0.335138236482335))
#
# })
