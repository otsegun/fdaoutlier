### model1 ####
test_that("Model1 works",{
  # model 1: normal operation, deterministic
  dtss <- simulation_model1(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50)
  expect_equal(dtss$true_outliers, c(11, 14, 20, 43, 53, 70, 79, 81, 83, 96))
  expect_equal(dim(dtss$data), c(100, 50))
  # model 1: normal operation, probabilistic
  dtss <- simulation_model1(100, 50, outlier_rate = .1, deterministic = F,
                            seed = 50)
  expect_equal(dtss$true_outliers, c(16, 22, 33, 65, 69, 76, 78, 89, 99))
  expect_equal(dim(dtss$data), c(100, 50))

  # model1: no outlier: deterministic
  dtss <- simulation_model1(100, 50, outlier_rate = 0, deterministic = T,
                            seed = 50)
  expect_equal(dtss$true_outliers, integer())
  expect_equal(dim(dtss$data), c(100, 50))
  # model1: no outlier: probabilistic
  dtss <- simulation_model1(100, 50, outlier_rate = 0, deterministic = F,
                            seed = 50)
  expect_equal(dtss$true_outliers, integer())
  expect_equal(dim(dtss$data), c(100, 50))


})

test_that("simulation_model1 plots when requested", {
  # plot shows
  dtss <- simulation_model1(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50, plot = T)
  # plot but not legend
  dtss <- simulation_model1(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50, plot = T, show_legend = F)
  # plot when there is no outlier
  dtss <- simulation_model1(100, 50, outlier_rate = 0, deterministic = T,
                            seed = 50, plot = T, show_legend = T)
})

### model2 ####
test_that("Model2 works", {
  # model2: no seed
  dtss <- simulation_model2(100, 50, outlier_rate = .1, deterministic = T)
  expect_equal(length(dtss$true_outliers),10)
  expect_equal(dim(dtss$data), c(100, 50))

  # model2: normal operation, deterministic
  dtss <- simulation_model2(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50)
  expect_equal(dtss$true_outliers, c(11, 14, 20, 43, 53, 70, 79, 81, 83, 96))
  expect_equal(dim(dtss$data), c(100, 50))
  # model2: normal operation, probabilistic
  dtss <- simulation_model2(100, 50, outlier_rate = .1, deterministic = F,
                            seed = 50)
  expect_equal(dtss$true_outliers, c(16, 22, 33, 65, 69, 76, 78, 89, 99))
  expect_equal(dim(dtss$data), c(100, 50))

  # model2: no outlier: deterministic
  dtss <- simulation_model2(100, 50, outlier_rate = 0, deterministic = T,
                            seed = 50)
  expect_equal(dtss$true_outliers, integer())
  expect_equal(dim(dtss$data), c(100, 50))
  # model2: no outlier: probabilistic
  dtss <- simulation_model2(100, 50, outlier_rate = 0, deterministic = F,
                            seed = 50)
  expect_equal(dtss$true_outliers, integer())
  expect_equal(dim(dtss$data), c(100, 50))
})



test_that("simulation_model2 plots when requested", {
  # plot shows
  dtss <- simulation_model2(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50, plot = T)
  # plot but not legend
  dtss <- simulation_model2(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50, plot = T, show_legend = F)
  # plot when there is no outlier
  dtss <- simulation_model2(100, 50, outlier_rate = 0, deterministic = T,
                            seed = 50, plot = T, show_legend = T)
})

### model3 ####

test_that("Model3 works", {
  # model3: no seed
  dtss <- simulation_model3(100, 50, outlier_rate = .1, deterministic = T)
  expect_equal(length(dtss$true_outliers),10)
  expect_equal(dim(dtss$data), c(100, 50))

  # model3: normal operation, deterministic
  dtss <- simulation_model3(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50)
  expect_equal(dtss$true_outliers, c(11, 14, 20, 43, 53, 70, 79, 81, 83, 96))
  expect_equal(dim(dtss$data), c(100, 50))
  # model3: normal operation, probabilistic
  dtss <- simulation_model3(100, 50, outlier_rate = .1, deterministic = F,
                            seed = 50)
  expect_equal(dtss$true_outliers, c(16, 22, 33, 65, 69, 76, 78, 89, 99))
  expect_equal(dim(dtss$data), c(100, 50))

  # model3: no outlier: deterministic
  dtss <- simulation_model3(100, 50, outlier_rate = 0, deterministic = T,
                            seed = 50)
  expect_equal(dtss$true_outliers, integer())
  expect_equal(dim(dtss$data), c(100, 50))
  # model3: no outlier: probabilistic
  dtss <- simulation_model3(100, 50, outlier_rate = 0, deterministic = F,
                            seed = 50)
  expect_equal(dtss$true_outliers, integer())
  expect_equal(dim(dtss$data), c(100, 50))


})



test_that("simulation_model3 plots when requested", {
  # plot shows
  dtss <- simulation_model3(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50, plot = T)
  # plot but not legend
  dtss <- simulation_model3(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50, plot = T, show_legend = F)
  # plot when there is no outlier
  dtss <- simulation_model3(100, 50, outlier_rate = 0, deterministic = T,
                            seed = 50, plot = T, show_legend = T)
})

### model4 ####

test_that("model4 works", {
  # model4: no seed
  dtss <- simulation_model4(100, 50, outlier_rate = .1, deterministic = T)
  expect_equal(length(dtss$true_outliers),10)
  expect_equal(dim(dtss$data), c(100, 50))

  # model4: normal operation, deterministic
  dtss <- simulation_model4(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50)
  expect_equal(dtss$true_outliers, c(11, 14, 20, 43, 53, 70, 79, 81, 83, 96))
  expect_equal(dim(dtss$data), c(100, 50))
  # model4: normal operation, probabilistic
  dtss <- simulation_model4(100, 50, outlier_rate = .1, deterministic = F,
                            seed = 50)
  expect_equal(dtss$true_outliers, c(16, 22, 33, 65, 69, 76, 78, 89, 99))
  expect_equal(dim(dtss$data), c(100, 50))

  # model4: no outlier: deterministic
  dtss <- simulation_model4(100, 50, outlier_rate = 0, deterministic = T,
                            seed = 50)
  expect_equal(dtss$true_outliers, integer())
  expect_equal(dim(dtss$data), c(100, 50))
  # model4: no outlier: probabilistic
  dtss <- simulation_model4(100, 50, outlier_rate = 0, deterministic = F,
                            seed = 50)
  expect_equal(dtss$true_outliers, integer())
  expect_equal(dim(dtss$data), c(100, 50))
})



test_that("simulation_model4 plots when requested", {
  # plot shows
  dtss <- simulation_model4(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50, plot = T)
  # plot but not legend
  dtss <- simulation_model4(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50, plot = T, show_legend = F)
  # plot when there is no outlier
  dtss <- simulation_model4(100, 50, outlier_rate = 0, deterministic = T,
                            seed = 50, plot = T, show_legend = T)
})

### model5 ####

test_that("model5 works", {
  # model5: no seed
  dtss <- simulation_model5(100, 50, outlier_rate = .1, deterministic = T)
  expect_equal(length(dtss$true_outliers),10)
  expect_equal(dim(dtss$data), c(100, 50))

  # model5: normal operation, deterministic
  dtss <- simulation_model5(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50)
  expect_equal(dtss$true_outliers, c(11, 14, 20, 43, 53, 70, 79, 81, 83, 96))
  expect_equal(dim(dtss$data), c(100, 50))
  # model5: normal operation, probabilistic
  dtss <- simulation_model5(100, 50, outlier_rate = .1, deterministic = F,
                            seed = 50)
  expect_equal(dtss$true_outliers, c(16, 22, 33, 65, 69, 76, 78, 89, 99))
  expect_equal(dim(dtss$data), c(100, 50))

  # model5: no outlier: deterministic
  dtss <- simulation_model5(100, 50, outlier_rate = 0, deterministic = T,
                            seed = 50)
  expect_equal(dtss$true_outliers, integer())
  expect_equal(dim(dtss$data), c(100, 50))
  # model5: no outlier: probabilistic
  dtss <- simulation_model5(100, 50, outlier_rate = 0, deterministic = F,
                            seed = 50)
  expect_equal(dtss$true_outliers, integer())
  expect_equal(dim(dtss$data), c(100, 50))


})



test_that("simulation_model5 plots when requested", {
  # plot shows
  dtss <- simulation_model5(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50, plot = T)
  # plot but not legend
  dtss <- simulation_model5(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50, plot = T, show_legend = F)
  # plot when there is no outlier
  dtss <- simulation_model5(100, 50, outlier_rate = 0, deterministic = T,
                            seed = 50, plot = T, show_legend = T)
})

### model6 ####
test_that("model6 works", {
  # model6: no seed
  dtss <- simulation_model6(100, 50, outlier_rate = .1, deterministic = T)
  expect_equal(length(dtss$true_outliers),10)
  expect_equal(dim(dtss$data), c(100, 50))

  # model6: normal operation, deterministic
  dtss <- simulation_model6(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50)
  expect_equal(dtss$true_outliers, c(11, 14, 20, 43, 53, 70, 79, 81, 83, 96))
  expect_equal(dim(dtss$data), c(100, 50))
  # model6: normal operation, probabilistic
  dtss <- simulation_model6(100, 50, outlier_rate = .1, deterministic = F,
                            seed = 50)
  expect_equal(dtss$true_outliers, c(16, 22, 33, 65, 69, 76, 78, 89, 99))
  expect_equal(dim(dtss$data), c(100, 50))

  # model6: no outlier: deterministic
  dtss <- simulation_model6(100, 50, outlier_rate = 0, deterministic = T,
                            seed = 50)
  expect_equal(dtss$true_outliers, integer())
  expect_equal(dim(dtss$data), c(100, 50))
  # model6: no outlier: probabilistic
  dtss <- simulation_model6(100, 50, outlier_rate = 0, deterministic = F,
                            seed = 50)
  expect_equal(dtss$true_outliers, integer())
  expect_equal(dim(dtss$data), c(100, 50))


})



test_that("simulation_model6 plots when requested", {
  # plot shows
  dtss <- simulation_model6(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50, plot = T)
  # plot but not legend
  dtss <- simulation_model6(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50, plot = T, show_legend = F)
  # plot when there is no outlier
  dtss <- simulation_model6(100, 50, outlier_rate = 0, deterministic = T,
                            seed = 50, plot = T, show_legend = T)
})


### model7 ####
test_that("model7 works", {
  # model7: no seed
  dtss <- simulation_model7(100, 50, outlier_rate = .1, deterministic = T)
  expect_equal(length(dtss$true_outliers),10)
  expect_equal(dim(dtss$data), c(100, 50))

  # model7: normal operation, deterministic
  dtss <- simulation_model7(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50)
  expect_equal(dtss$true_outliers, c(11, 14, 20, 43, 53, 70, 79, 81, 83, 96))
  expect_equal(dim(dtss$data), c(100, 50))
  # model7: normal operation, probabilistic
  dtss <- simulation_model7(100, 50, outlier_rate = .1, deterministic = F,
                            seed = 50)
  expect_equal(dtss$true_outliers, c(16, 22, 33, 65, 69, 76, 78, 89, 99))
  expect_equal(dim(dtss$data), c(100, 50))

  # model7: no outlier: deterministic
  dtss <- simulation_model7(100, 50, outlier_rate = 0, deterministic = T,
                            seed = 50)
  expect_equal(dtss$true_outliers, integer())
  expect_equal(dim(dtss$data), c(100, 50))
  # model7: no outlier: probabilistic
  dtss <- simulation_model7(100, 50, outlier_rate = 0, deterministic = F,
                            seed = 50)
  expect_equal(dtss$true_outliers, integer())
  expect_equal(dim(dtss$data), c(100, 50))


})



test_that("simulation_model7 plots when requested", {
  # plot shows
  dtss <- simulation_model7(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50, plot = T)
  # plot but not legend
  dtss <- simulation_model7(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50, plot = T, show_legend = F)
  # plot when there is no outlier
  dtss <- simulation_model7(100, 50, outlier_rate = 0, deterministic = T,
                            seed = 50, plot = T, show_legend = T)
})

### model8 ####
test_that("model8 works", {
  # model8: no seed
  dtss <- simulation_model8(100, 50, outlier_rate = .1, deterministic = T)
  expect_equal(length(dtss$true_outliers),10)
  expect_equal(dim(dtss$data), c(100, 50))

  # model8: normal operation, deterministic
  dtss <- simulation_model8(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50)
  expect_equal(dtss$true_outliers, c(11, 14, 20, 43, 53, 70, 79, 81, 83, 96))
  expect_equal(dim(dtss$data), c(100, 50))
  # model8: normal operation, probabilistic
  dtss <- simulation_model8(100, 50, outlier_rate = .1, deterministic = F,
                            seed = 50)
  expect_equal(dtss$true_outliers, c(16, 22, 33, 65, 69, 76, 78, 89, 99))
  expect_equal(dim(dtss$data), c(100, 50))

  # model8: no outlier: deterministic
  dtss <- simulation_model8(100, 50, outlier_rate = 0, deterministic = T,
                            seed = 50)
  expect_equal(dtss$true_outliers, integer())
  expect_equal(dim(dtss$data), c(100, 50))
  # model8: no outlier: probabilistic
  dtss <- simulation_model8(100, 50, outlier_rate = 0, deterministic = F,
                            seed = 50)
  expect_equal(dtss$true_outliers, integer())
  expect_equal(dim(dtss$data), c(100, 50))


})



test_that("simulation_model8 plots when requested", {
  # plot shows
  dtss <- simulation_model8(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50, plot = T)
  # plot but not legend
  dtss <- simulation_model8(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50, plot = T, show_legend = F)
  # plot when there is no outlier
  dtss <- simulation_model8(100, 50, outlier_rate = 0, deterministic = T,
                            seed = 50, plot = T, show_legend = T)
})

### model9 ####
test_that("model9 works", {
  # model9: no seed
  dtss <- simulation_model9(100, 50, outlier_rate = .1, deterministic = T)
  expect_equal(length(dtss$true_outliers),10)
  expect_equal(dim(dtss$data), c(100, 50))

  # model9: normal operation, deterministic
  dtss <- simulation_model9(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50)
  expect_equal(dtss$true_outliers, c(11, 14, 20, 43, 53, 70, 79, 81, 83, 96))
  expect_equal(dim(dtss$data), c(100, 50))
  # model9: normal operation, probabilistic
  dtss <- simulation_model9(100, 50, outlier_rate = .1, deterministic = F,
                            seed = 50)
  expect_equal(dtss$true_outliers, c(16, 22, 33, 65, 69, 76, 78, 89, 99))
  expect_equal(dim(dtss$data), c(100, 50))

  # model9: no outlier: deterministic
  dtss <- simulation_model9(100, 50, outlier_rate = 0, deterministic = T,
                            seed = 50)
  expect_equal(dtss$true_outliers, integer())
  expect_equal(dim(dtss$data), c(100, 50))
  # model9: no outlier: probabilistic
  dtss <- simulation_model9(100, 50, outlier_rate = 0, deterministic = F,
                            seed = 50)
  expect_equal(dtss$true_outliers, integer())
  expect_equal(dim(dtss$data), c(100, 50))


})



test_that("simulation_model9 plots when requested", {
  # plot shows
  dtss <- simulation_model9(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50, plot = T)
  # plot but not legend
  dtss <- simulation_model9(100, 50, outlier_rate = .1, deterministic = T,
                            seed = 50, plot = T, show_legend = F)
  # plot when there is no outlier
  dtss <- simulation_model9(100, 50, outlier_rate = 0, deterministic = T,
                            seed = 50, plot = T, show_legend = T)
})


# ### model10 ####
# test_that("model10 works", {
#   # model10: no seed
#   dtss <- simulation_model10(100, 50, outlier_rate = .1, deterministic = T)
#   expect_equal(length(dtss$true_outliers),10)
#   expect_equal(dim(dtss$data), c(100, 50))
#
#   # model10: normal operation, deterministic
#   dtss <- simulation_model10(100, 50, outlier_rate = .1, deterministic = T,
#                             seed = 50)
#   expect_equal(dtss$true_outliers, c(11, 14, 20, 43, 53, 70, 79, 81, 83, 96))
#   expect_equal(dim(dtss$data), c(100, 50))
#   # model10: normal operation, probabilistic
#   dtss <- simulation_model10(100, 50, outlier_rate = .1, deterministic = F,
#                             seed = 50)
#   expect_equal(dtss$true_outliers, c(16, 22, 33, 65, 69, 76, 78, 89, 99))
#   expect_equal(dim(dtss$data), c(100, 50))
#
#   # model10: no outlier: deterministic
#   dtss <- simulation_model10(100, 50, outlier_rate = 0, deterministic = T,
#                             seed = 50)
#   expect_equal(dtss$true_outliers, integer())
#   expect_equal(dim(dtss$data), c(100, 50))
#   # model10: no outlier: probabilistic
#   dtss <- simulation_model10(100, 50, outlier_rate = 0, deterministic = F,
#                             seed = 50)
#   expect_equal(dtss$true_outliers, integer())
#   expect_equal(dim(dtss$data), c(100, 50))
#
#
# })
#
#
#
# test_that("simulation_model10 plots when requested", {
#   # plot shows
#   dtss <- simulation_model10(100, 50, outlier_rate = .1, deterministic = T,
#                             seed = 50, plot = T)
#   # plot but not legend
#   dtss <- simulation_model10(100, 50, outlier_rate = .1, deterministic = T,
#                             seed = 50, plot = T, show_legend = F)
#   # plot when there is no outlier
#   dtss <- simulation_model10(100, 50, outlier_rate = 0, deterministic = T,
#                             seed = 50, plot = T, show_legend = T)
# })


### seeds work ####
test_that("seeds work", {
  # model1 deterministic
  dtss <- simulation_model1(10, 50, outlier_rate = .1, deterministic = T, seed = 50)
  dtss2 <- simulation_model1(10, 50, outlier_rate = .1, deterministic = T, seed = 50)
  expect_equal(dtss, dtss2)

  # model1 probabilistic
  dtss3 <- simulation_model1(10, 50, outlier_rate = .5, deterministic = F, seed = 50)
  dtss4 <- simulation_model1(10, 50, outlier_rate = .5, deterministic = F, seed = 50)
  expect_equal(dtss3, dtss4)

  # model2 deterministic
  dtss <- simulation_model2(10, 50, outlier_rate = .1, deterministic = T,
                            seed = 50)
  dtss2 <- simulation_model2(10, 50, outlier_rate = .1, deterministic = T,
                             seed = 50)
  expect_equal(dtss, dtss2)

  # model2 probabilistic
  dtss3 <- simulation_model2(10, 50, outlier_rate = .5, deterministic = F,
                             seed = 50)
  dtss4 <- simulation_model2(10, 50, outlier_rate = .5, deterministic = F,
                             seed = 50)
  expect_equal(dtss3, dtss4)

  # model3 deterministic
  dtss <- simulation_model3(10, 50, outlier_rate = .1, deterministic = T,
                            seed = 50)
  dtss2 <- simulation_model3(10, 50, outlier_rate = .1, deterministic = T,
                             seed = 50)
  expect_equal(dtss, dtss2)

  # model3 probabilistic
  dtss3 <- simulation_model3(10, 50, outlier_rate = .5, deterministic = F,
                             seed = 50)
  dtss4 <- simulation_model3(10, 50, outlier_rate = .5, deterministic = F,
                             seed = 50)
  expect_equal(dtss3, dtss4)

  # model4 deterministic
  dtss <- simulation_model4(10, 50, outlier_rate = .1, deterministic = T,
                            seed = 50)
  dtss2 <- simulation_model4(10, 50, outlier_rate = .1, deterministic = T,
                             seed = 50)
  expect_equal(dtss, dtss2)

  # model4 probabilistic
  dtss3 <- simulation_model4(10, 50, outlier_rate = .5, deterministic = F,
                             seed = 50)
  dtss4 <- simulation_model4(10, 50, outlier_rate = .5, deterministic = F,
                             seed = 50)
  expect_equal(dtss3, dtss4)

  # model5 deterministic
  dtss <- simulation_model5(10, 50, outlier_rate = .1, deterministic = T,
                            seed = 50)
  dtss2 <- simulation_model5(10, 50, outlier_rate = .1, deterministic = T,
                             seed = 50)
  expect_equal(dtss, dtss2)

  # model5 probabilistic
  dtss3 <- simulation_model5(10, 50, outlier_rate = .5, deterministic = F,
                             seed = 50)
  dtss4 <- simulation_model5(10, 50, outlier_rate = .5, deterministic = F,
                             seed = 50)
  expect_equal(dtss3, dtss4)

  # model6 deterministic
  dtss <- simulation_model6(10, 50, outlier_rate = .1, deterministic = T,
                            seed = 50)
  dtss2 <- simulation_model6(10, 50, outlier_rate = .1, deterministic = T,
                             seed = 50)
  expect_equal(dtss, dtss2)

  # model6 probabilistic
  dtss3 <- simulation_model6(10, 50, outlier_rate = .5, deterministic = F,
                             seed = 50)
  dtss4 <- simulation_model6(10, 50, outlier_rate = .5, deterministic = F,
                             seed = 50)
  expect_equal(dtss3, dtss4)

  # model7 deterministic
  dtss <- simulation_model7(10, 50, outlier_rate = .1, deterministic = T,
                            seed = 50)
  dtss2 <- simulation_model7(10, 50, outlier_rate = .1, deterministic = T,
                             seed = 50)
  expect_equal(dtss, dtss2)

  # model7 probabilistic
  dtss3 <- simulation_model7(10, 50, outlier_rate = .5, deterministic = F,
                             seed = 50)
  dtss4 <- simulation_model7(10, 50, outlier_rate = .5, deterministic = F,
                             seed = 50)
  expect_equal(dtss3, dtss4)

  # model8 deterministic
  dtss <- simulation_model8(10, 50, outlier_rate = .1, deterministic = T,
                            seed = 50)
  dtss2 <- simulation_model8(10, 50, outlier_rate = .1, deterministic = T,
                             seed = 50)
  expect_equal(dtss, dtss2)

  # model8 probabilistic
  dtss3 <- simulation_model8(10, 50, outlier_rate = .5, deterministic = F,
                             seed = 50)
  dtss4 <- simulation_model8(10, 50, outlier_rate = .5, deterministic = F,
                             seed = 50)
  expect_equal(dtss3, dtss4)

  # model9 deterministic
  dtss <- simulation_model9(10, 50, outlier_rate = .1, deterministic = T,
                            seed = 50)
  dtss2 <- simulation_model9(10, 50, outlier_rate = .1, deterministic = T,
                             seed = 50)
  expect_equal(dtss, dtss2)

  # model9 probabilistic
  dtss3 <- simulation_model9(10, 50, outlier_rate = .5, deterministic = F,
                             seed = 50)
  dtss4 <- simulation_model9(10, 50, outlier_rate = .5, deterministic = F,
                             seed = 50)
  expect_equal(dtss3, dtss4)

  # # model10 deterministic
  # dtss <- simulation_model10(10, 50, outlier_rate = .1, deterministic = T,
  #                           seed = 50)
  # dtss2 <- simulation_model10(10, 50, outlier_rate = .1, deterministic = T,
  #                            seed = 50)
  # expect_equal(dtss, dtss2)
  #
  # # model10 probabilistic
  # dtss3 <- simulation_model10(10, 50, outlier_rate = .5, deterministic = F,
  #                            seed = 50)
  # dtss4 <- simulation_model10(10, 50, outlier_rate = .5, deterministic = F,
  #                            seed = 50)
  # expect_equal(dtss3, dtss4)



})
