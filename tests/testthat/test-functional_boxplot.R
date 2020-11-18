# test_that("functional_boxplot handles unconventional data", {
#   expect_error(functional_boxplot(dt = list()))
#   expect_error(functional_boxplot(dt = data.frame()) )
#   expect_error(directional_quantile(dt = matrix(NA, nrow = 1, ncol = 3)))
#   expect_error(directional_quantile(dt = matrix(c(1:2, NA), nrow = 1, ncol = 3)))
# })
#
# test_that("functional_boxplot uses the appropriate depth methods when specified", {
#   # data setup
#   depth_method = c("mbd", "tvd", "extremal", "dirout",
#                    "linfinity", "bd", "erld", "dq")
#
#   # mbd
#   bimbo <- functional_boxplot(sim_data1$data, depth_method = "mbd")
#   expect_equal(bimbo$median_curve, 32)
#   expect_equal(bimbo$depth_values[1:10],
#                c(0.338165656565657, 0.386529292929294, 0.312315151515152, 0.040727272727273,
#                0.356016161616162, 0.439305050505052, 0.353624242424243,
#                0.287200000000001, 0.457543434343435, 0.488371717171718))
#
#   # tvd
#   bimbo <- functional_boxplot(sim_data1$data, depth_method = "tvd")
#   expect_equal(bimbo$median_curve, 32)
#   expect_equal(bimbo$depth_values[1:10],
#                c(0.159546, 0.184112, 0.147108, .020054,
#                  .168548, .212006, 0.167466,
#                  0.140402, 0.220646, 0.237636))
#
#   # extremal
#   bimbo <- functional_boxplot(sim_data1$data, depth_method = "extremal")
#   expect_equal(bimbo$median_curve, 99)
#   expect_equal(bimbo$depth_values[1:10],
#                c(0.55, 0.59, 0.16, 0.03, 0.54,
#                  0.50, 0.36, 0.20, 0.67, 0.97))
#
#
#   # dirout
#   bimbo <- functional_boxplot(sim_data1$data, depth_method = "dirout")
#   expect_equal(bimbo$median_curve, 11)
#   expect_equal(bimbo$depth_values[1:10],
#                c(-2.193691360000715, -0.999661532178892, -9.067971747412946, -76.298305683645552, -2.416867771561302,
#                  -0.393584284337596, -1.166243224919002, -4.314272721323127, -0.178377388160415, -1.334467540951053))
#
#   # linfinity
#   bimbo <- functional_boxplot(sim_data1$data, depth_method = "linfinity")
#   expect_equal(bimbo$median_curve, 27)
#   expect_equal(bimbo$depth_values[1:10],
#                c(0.263327367005006, 0.265193605694392, 0.224142493628897, 0.098979751499347, 0.264591053329429,
#                  0.261223930416223, 0.256972045714086, 0.227730751187658, 0.272384400470710, 0.282187675351543))
#
#   # bd
#   bimbo <- functional_boxplot(sim_data1$data, depth_method = "linfinity")
#   expect_equal(bimbo$median_curve, 27)
#   expect_equal(bimbo$depth_values[1:10],
#                c(0.263327367005006, 0.265193605694392, 0.224142493628897, 0.098979751499347, 0.264591053329429,
#                  0.261223930416223, 0.256972045714086, 0.227730751187658, 0.272384400470710, 0.282187675351543))
#
#   # erld
#   expect_warning(bimbo <- functional_boxplot(sim_data1$data, depth_method = "erld"))
#   expect_equal(bimbo$median_curve, 99)
#   expect_equal(bimbo$depth_values[1:25],
#                c(0.55, 0.60, 0.17, 0.03, 0.54, 0.52, 0.38, 0.21,
#                  0.67, 0.97, 0.84, 0.07, 0.28, 0.14, 0.90, 0.94,
#                  0.04, 0.29, 0.22, 0.32, 0.59, 0.79, 0.05, 0.57,
#                  0.27))
#
#   # dq
#   expect_warning(bimbo <- functional_boxplot(sim_data1$data, depth_method = "dq"))
#   expect_equal(bimbo$depth_values[1:10],
#                -c(0.216681000024687, 0.192288211203389, 0.323896564320357, 1.303701740305201, 0.190073991166244,
#                   0.203276318023945, 0.233342740948986, 0.304819325981823, 0.186640729721500, 0.100542638053176))
#
#
# })
#
# test_that("functional boxplot handles strange depth_method", {
#   expect_error(functional_boxplot(sim_data1$data, depth_method = "dqs"))
#
# })
#
# test_that("functional boxplot protests when length of depth values is not equal to nrow of dt", {
#   expect_error(functional_boxplot(sim_data1$data, depth_values = 1:5))
# })
#
# test_that("functional boxplot objects to wierd central region.", {
#   expect_error(functional_boxplot(sim_data1$data, central_region = 1))
# })
#
#
# test_that("functional boxplot uses depth values when supplied", {
#   duskyduck <- c(0.55, 0.60, 0.17, 0.03, 0.54, 0.52, 0.38, 0.21,
#                  0.67, 0.97, 0.84, 0.07, 0.28, 0.14, 0.90, 0.94,
#                  0.04, 0.29, 0.22, 0.32, 0.59, 0.79, 0.05, 0.57,
#                  0.27)
#   bimbo <- functional_boxplot(sim_data1$data[1:length(duskyduck), ],
#                               depth_values = duskyduck)
#   expect_equal(bimbo$median_curve, tail(order(duskyduck), 1))
#   expect_equal(bimbo$depth_values, duskyduck)
#   expect_equal(bimbo$outliers, c(4, 12, 17, 23))
#
#
# })
#
#
#
#
#
#
#
