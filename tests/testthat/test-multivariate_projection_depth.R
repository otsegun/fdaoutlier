test_that("projection_depth handles unconventional inputs, non-matrices,  and non-dataframe objects", {
  expect_error(projection_depth(dts = list()) )
})

# correctness tested Sep 12, 2020
test_that("projection_depth produces correct results", {
  data("iris")
  dts <- iris[1:5, -5]
  dt <- iris[1:10, -5]
  test_depth <- projection_depth(dts = dts, dt = dt, n_projections = 7, seed = 29)

  # that a numeric vector is returned
  expect_is(test_depth, "numeric")

  # correctness

  expect_equal(test_depth, c(0.43706257729447, 0.704065758724527,
                             0.500000000000000, 0.500000000000000,
                             0.477294075885465))

  # works for a vector
  test_depth2 <- projection_depth(dts = unlist(unname(dts[1, ])),
                                  dt = dt, n_projections = 7, seed = 29)
  expect_equal(test_depth2, test_depth[1])

})


test_that("projection_depth throws error for dts of different dimension", {
  data("iris")

  dts <- iris[1:5, -(4:5)] # 5 by 3
  dt <- iris[1:10, -5] # 10 by 4
  #matrix
  expect_error(projection_depth(dts = dts, dt = dt, n_projections = 5, seed = 20))

  # works for a vector
  expect_error(projection_depth(dts = unlist(unname(dts[1, ])),
                                  dt = dt, n_projections = 5, seed = 20))

})


test_that("projection_depth throws error for non-numeric data", {
  data("iris")

  dts <- iris[1:5, ]
  dt <- iris[1:10, ]
  #matrix
  expect_error(projection_depth(dts = dts, dt = dt, n_projections = 50, seed = 29))

  # works for a vector
  expect_error(projection_depth(dts = unlist(unname(dts[1, ])),
                                dt = dt, n_projections = 50, seed = 29))

})

test_that("projection_depth rejects NA values", {
  data("iris")

  dts <- iris[1:5, -5]
  dt <- iris[1:10, -5]
  dts[1,3] <- NA
  #matrix
  expect_error(projection_depth(dts = dts, dt = dt, n_projections = 50, seed = 29),
               "Missing or infinite values are not allowed in arguments \"dt\" and \"dts\".")

  # works for a vector
  expect_error(projection_depth(dts = unlist(unname(dts[1, ])),
                                dt = dt, n_projections = 50, seed = 29),
               "Missing or infinite values are not allowed in arguments \"dt\" and \"dts\".")

})
