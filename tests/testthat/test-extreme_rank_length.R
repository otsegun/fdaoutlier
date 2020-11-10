test_that("extreme_rank_lenk depth handles unconventional data", {
  expect_error(extreme_rank_length(dt = list()))
  expect_error(extreme_rank_length(dt = data.frame()) )
  expect_error(extreme_rank_length(dt = matrix(0, nrow = 1, ncol = 3)))
  expect_error(extreme_rank_length(dt = matrix(NA, nrow = 1, ncol = 3)))
})

test_that("extreme_rank_lenk depth throws errows on strange type", {
  expect_error(extreme_rank_length(dt = matrix(1:9, nrow = 3, ncol = 3), type = "one_sided_laft"))
})

test_that("extreme_rank_lenk depth produces correct results", {
  dt <- sim_data1$data[1:9, 1:7]
  dtt <- rbind(dt, dt[1,])

  # right
  expect_equal(extreme_rank_length(dt, type = "one_sided_right")*9,
               c(5, 4, 2, 9, 3, 6, 1, 8, 7))

  expect_equal(extreme_rank_length(dtt, type = "one_sided_right"),
               c(0.55, 0.40, 0.20, 1.00, 0.30, 0.70, 0.10, 0.90, 0.80, 0.55))
  #two
  expect_equal(extreme_rank_length(dt, type = "two_sided")*9,
               c(9, 8, 3, 1, 4, 7, 2, 5, 6))
  expect_equal(extreme_rank_length(dtt, type = "two_sided"),
               c(0.95, 0.80, 0.30, 0.10, 0.40, 0.70, 0.20, 0.50, 0.60, 0.95))
  # left
  expect_equal(extreme_rank_length(dt, type = "one_sided_left")*9,
               c(6, 7, 8, 1, 5, 4, 9, 2, 3))
  expect_equal(extreme_rank_length(dtt, type = "one_sided_left"),
               c(0.75, 0.60, 0.90, 0.10, 0.50, 0.40, 1.00, 0.20, 0.30, 0.75))


})
