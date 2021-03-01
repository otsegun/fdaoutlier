test_that("functional_boxplot handles unconventional data", {
  expect_error(functional_boxplot(dt = list()))
  expect_error(functional_boxplot(dt = data.frame()) )
  expect_error(directional_quantile(dt = matrix(NA, nrow = 1, ncol = 3)))
  expect_error(directional_quantile(dt = matrix(c(1:2, NA), nrow = 1, ncol = 3)))
})
dt1 <- simulation_model1(seed = 50)
test_that("functional_boxplot uses the appropriate depth methods when specified", {
  # data setup
  depth_method = c("mbd", "tvd", "extremal", "dirout",
                   "linfinity", "bd", "erld", "dq")

  # mbd

  bimbo <- functional_boxplot(dt1$data, depth_method = "mbd")
  expect_equal(bimbo$median_curve, 47)
  expect_equal(bimbo$depth_values[1:10],
               c(0.490165656565657, 0.331296969696970, 0.141745454545455,
                 0.365260606060606, 0.298553535353535, 0.313882828282828,
                 0.394529292929293, 0.225357575757576, 0.363175757575758,
                 0.428282828282828))

  # tvd
  bimbo <- functional_boxplot(dt1$data, depth_method = "tvd")
  expect_equal(bimbo$median_curve, 47)
  expect_equal(bimbo$depth_values[1:10],
               c(0.238044000000000, 0.161746000000000, 0.069496000000000,
                 0.177122000000000, 0.139762000000000, 0.148084000000000,
                 0.192090000000000, 0.102956000000000, 0.173320000000000,
                 0.208754000000000))

  # extremal
  bimbo <- functional_boxplot(dt1$data, depth_method = "extremal")
  expect_equal(bimbo$median_curve, 47)
  expect_equal(bimbo$depth_values[1:10],
               c(0.85, 0.52, 0.20, 0.54, 0.33, 0.28, 0.63, 0.07, 0.09, 0.78))


  # dirout
  bimbo <- functional_boxplot(dt1$data, depth_method = "dirout")
  expect_equal(bimbo$median_curve, 34)
  expect_equal(bimbo$depth_values[1:10],
               c(-0.562633516580735, -3.093025592112852, -4.486081211405466,
                 -58.722387790349124, -2.341938490160342, -37.670909546633808,
                 -3.126579237640941, -11.128940161596638, -85.189542384597203,
                 -0.300964241736215))

  # linfinity
  bimbo <- functional_boxplot(dt1$data, depth_method = "linfinity")
  expect_equal(bimbo$median_curve, 47)
  expect_equal(bimbo$depth_values[1:10],
               c(0.297149957050392, 0.265360097783553, 0.224017761563037,
                 0.262569271637305, 0.271507888410369, 0.247680032267054,
                 0.275767195030767, 0.243584235304531, 0.242618743203875,
                 0.288253333578114))

  # bd
  bimbo <- functional_boxplot(dt1$data, depth_method = "bd")
  expect_equal(bimbo$median_curve, 90)
  expect_equal(bimbo$depth_values[1:10],
               c(0.143636363636364, 0.098181818181818, 0.086262626262626,
                 0.054545454545455, 0.073535353535354, 0.033737373737374,
                 0.057777777777778, 0.045454545454545, 0.030909090909091,
                 0.138181818181818))

  # erld
  expect_warning(bimbo <- functional_boxplot(dt1$data, depth_method = "erld"))
  expect_equal(bimbo$median_curve, 47)
  expect_equal(bimbo$depth_values[1:25],
               c(0.86, 0.52, 0.20, 0.54, 0.33, 0.28, 0.63, 0.08,
                 0.09, 0.78, 0.94, 0.43, 0.66, 0.06, 0.79, 0.24,
                 0.72, 0.25, 0.27, 0.04, 0.55, 0.30, 0.93, 0.85,
                 0.99))

  # dq
  expect_warning(bimbo <- functional_boxplot(dt1$data, depth_method = "dq"))
  expect_equal(bimbo$depth_values[1:10],
               -c(0.290588513754194, 0.228980483181792, 0.354149647826547,
                  0.454072541473456, 0.787515643937046, 0.934328308186221,
                  0.535256076349059, 1.085520808291799, 1.066174541009852,
                  0.213075768498317))

})

test_that("functional boxplot handles strange depth_method", {
  expect_error(functional_boxplot(dt1$data, depth_method = "dqs"))

})

test_that("functional boxplot protests when length of depth values is not equal to nrow of dt", {
  expect_error(functional_boxplot(dt1$data, depth_values = 1:5))
})

test_that("functional boxplot objects to wierd central region.", {
  expect_error(functional_boxplot(dt1$data, central_region = 1))
})


test_that("functional boxplot uses depth values when supplied", {
  duskyduck <- c(0.86, 0.52, 0.20, 0.54, 0.33, 0.28, 0.63, 0.08,
                 0.09, 0.78, 0.94, 0.43, 0.66, 0.06, 0.79, 0.24,
                 0.72, 0.25, 0.27, 0.04, 0.55, 0.30, 0.93, 0.85,
                 0.99)
  bimbo <- functional_boxplot(dt1$data[1:length(duskyduck), ],
                              depth_values = duskyduck)
  expect_equal(bimbo$median_curve, tail(order(duskyduck), 1))
  expect_equal(bimbo$depth_values, duskyduck)
  expect_equal(bimbo$outliers, 20)


})







