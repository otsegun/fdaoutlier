# tested with fda.usc::mdepth.RP on 09-09-20 by Segun
test_that("projection_depth handles unconventional inputs, non-matrices,  and non-dataframe objects", {
  expect_error(projection_depth(dts = list()) )
})

test_that("projection_depth produces correct results", {
  data("iris")
  dts <- iris[1:5, -5]
  dt <- iris[1:10, -5]
  test_depth <- projection_depth(dts = dts, dt = dt, n_projections = 50, seed = 29)

  # that a numeric vector is returned
  expect_is(test_depth, "numeric")

  # correctness
  #tmp_projection_depth <- "tests/reference_files/ref_mprojection_depth.rda"
  #expect_known_value(test_depth, tmp_projection_depth, update = T, print = TRUE)
  expect_known_value(test_depth, "../reference_files/ref_mprojection_depth.rda",
                     update = F, print = TRUE)

  # works for a vector
  test_depth2 <- projection_depth(dts = unlist(unname(dts[1, ])),
                                  dt = dt, n_projections = 50, seed = 29)
  expect_equal(test_depth2, test_depth[1])

})


test_that("projection_depth throws error for dts of different dimension", {
  data("iris")

  dts <- iris[1:5, -(4:5)]
  dt <- iris[1:10, -5]
  #matrix
  expect_error(projection_depth(dts = dts, dt = dt, n_projections = 50, seed = 29))

  # works for a vector
  expect_error(projection_depth(dts = unlist(unname(dts[1, ])),
                                  dt = dt, n_projections = 50, seed = 29))

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
