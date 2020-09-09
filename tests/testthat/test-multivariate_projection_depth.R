# tested with fda.usc::mdepth.RP on 09-09-20 by Segun
test_that("projection_depth handles unconventional inputs, non-matrices,  and non-dataframe objects", {
  expect_error(projection_depth(data = list()) )
})

test_that("projection_depth produces correct results", {
  dt <- iris[1:5, -5]
  test_depth <- projection_depth(dt = dt, n_projections = 500, seed = 29)

  # that a numeric vector is returned
  expect_is(test_depth, "numeric")

  # correctness
  #tmp_projection_depth <- "tests/reference_files/ref_mprojection_depth.rda"
  #expect_known_value(test_depth, tmp_projection_depth, update = F, print = TRUE)
  expect_known_value(test_depth, "../reference_files/ref_mprojection_depth.rda",
                     update = F, print = TRUE)
})
