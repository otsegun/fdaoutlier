test_that("dir_out handles unconventional inputs, non-matrices,  and non-array objects", {
  expect_error(dir_out(dts = list()) )
  expect_error(dir_out(dts = data.frame()) )
})

multiv_data <- c(-1.00699287, -0.53823436, -0.55879513, -0.18606117, -0.52050693,
                 0.46096140, -0.32240037, -0.46775918, -0.38068162, 0.02840495,
                 -0.16316400,  0.18996883, -0.25430960, -0.06389951, -0.46119745,
                 -0.30353525, -1.21554894, -1.22182643, -0.63857941, -0.84320227,
                 -0.77277610, -0.88696090, -0.80527829, -0.33013994, -0.25724301,
                 0.86073945, -0.75379286, -0.81968759, -0.57461555, -1.02516021)

multiv_data <- array(multiv_data, dim = c(5, 3, 2))
univ_data <- multiv_data[,,1]

test_that("dir_out produces correct results", {


  multiv_result <- dir_out(dts = multiv_data, return_dir_matrix = T,
                           data_depth = "random_projections",
                           n_projections = 500,
                           seed = 20,
                           return_distance = T)
  # multiv_result2 <- dir_out(dts = multiv_data, return_dir_matrix = T,
  #                           data_depth = "random_projections2",
  #                           n_projections = 500,
  #                           seed = 20,
  #                           return_distance = T)

  univ_result <- dir_out(dts = univ_data, return_dir_matrix = T,
                         data_depth = "random_projections", return_distance = T)

  # that a list a returned
  expect_is(multiv_result,  "list")
  expect_is(univ_result, "list")

  # that randomprojection and randomprojection2 are the same
  #expect_equal(multiv_result2, multiv_result)

  ## univariate correctness

  # distance
  expect_equal(univ_result$distance,
                     c(659.832198598165974, 2.105766012219121, 1.385929916565913,
                       2.240298160329774, 0.268005910885199))

  # directional outlyingness matrix
  expect_equal(as.vector(univ_result$dirout_matrix),
               c(-15.377502079008568, 0.000000000000000, -0.674490759476595,
                 11.552950711009132, 0.581543771185037, 3.634938090042357,
                 0.000000000000000, -0.674490759476595, -0.270435376952696,
                 1.627799145543569, 0.000000000000000, 2.399496846282285,
                 -0.619323838433562, 0.674490759476595, -2.025102914848300))

  # mean outlyingness
  expect_equal(univ_result$mean_outlyingness,
               c(-3.914187996322071, 0.799832282094095, -0.656101785795584,
                 3.985668697844344, 0.061413333960102))

  # var outlyingness
  expect_equal(univ_result$var_outlyingness,
                     c(101.858871048349130, 1.919195038439544, 0.001014463059123,
                       43.171039150902288, 3.538825119499641))

  ## multivariate correctness
  # that distance is correct
  expect_equal(as.vector(multiv_result$dirout_matrix),
               c(-27.584641886244484, 0.000000000000000, -1.131860215867366,
                 14.137796121261358, 0.548886661925531, 7.781292812386559,
                 1.576129860293521, 0.000000000000000, 0.976476696502556,
                 4.079418708103010, 4.155220108088729, 20.331063342345377,
                 0.000000000000000, 0.838409494802724, -2.157791727050806,
                 53.668510538619131, 0.000000000000000, -0.345572718653301,
                 23.162119704001281, 11.528807095861456, 0.272319859040727,
                 -0.885686947270071, 0.000000000000000, 5.328141100757476,
                 4.505898832171668, 76.608681349226089, 3.015473718939599,
                 0.000000000000000, 1.079095783457027, -2.143031210249678))

  # mean outlyingness
  expect_equal(as.vector(multiv_result$mean_outlyingness),
                     c(-5.216042988589732, 7.302397734212966, -0.377286738622455,
                       5.317560770855546, 0.823504547659245, 43.516503915628647 ,
                       0.709928923889843, -0.115190906217767, 9.856452196071928,
                       4.630558239261148))
  # var outlyingness
  expect_equal(multiv_result$var_outlyingness,
                     c(1275.106809028220596, 88.075603282768895, 0.311228456031279,
                       130.430911177500690, 37.682470255610284))
})


test_that("dir_out arguments return_dir_matrix and return_distance works well", {

    multiv_result_test3 <- dir_out(dts = multiv_data, return_dir_matrix = F,
                           data_depth = "random_projections",
                           n_projections = 3,
                           seed = NULL,
                           return_distance = T)
    expect_null(multiv_result_test3$dirout_matrix)

    multiv_result_test4 <- dir_out(dts = multiv_data, return_dir_matrix = F,
                                   data_depth = "random_projections",
                                   n_projections = 3,
                                   seed = NULL,
                                   return_distance = F)
    expect_null(multiv_result_test4$dirout_matrix)
    expect_null(multiv_result_test4$distance)
    expect_null(multiv_result_test4$mcd_obj)
    expect_null(multiv_result_test4$msmatrix)



})




