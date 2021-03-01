dt1 <- simulation_model1(seed = 50)
test_that("extreme_rank_lenk depth handles unconventional data", {
  expect_error(extreme_rank_length(dt = list()))
  expect_error(extreme_rank_length(dt = data.frame()) )
  expect_error(extreme_rank_length(dt = matrix(0, nrow = 1, ncol = 3)))
  #expect_error(extreme_rank_length(dt = matrix(NA, nrow = 1, ncol = 3)))
  expect_error(extreme_rank_length(dt = matrix(c(1:2, NA), nrow = 1, ncol = 3)))
  expect_error(extreme_rank_length(dt = dt1$data, type = "left"))
})

test_that("extreme_rank_lenk depth throws errows on strange type", {
  expect_error(extreme_rank_length(dt = matrix(1:9, nrow = 3, ncol = 3),
                                   type = "one_sided_laft"))
})



test_that("extreme_rank_lenk depth produces correct results", {

  dt <-dt1$data[1:9, 1:7]
  dtt <- rbind(dt, dt[1,])

  # right
  expect_equal(extreme_rank_length(dt, type = "one_sided_right")*9,
               c(4, 7, 8, 9, 2, 6, 3, 1, 5))

  expect_equal(extreme_rank_length(dtt, type = "one_sided_right"),
               c(0.45, 0.80, 0.90, 1.00, 0.20, 0.70, 0.30, 0.10, 0.60, 0.45))
  #two
  expect_equal(extreme_rank_length(dt, type = "two_sided")*9,
               c(8, 6, 3, 2, 4, 7, 5, 1, 9))
  expect_equal(extreme_rank_length(dtt, type = "two_sided"),
               c(0.85, 0.60, 0.30, 0.20, 0.40, 0.70, 0.50, 0.10, 1.00, 0.85))
  # left
  expect_equal(extreme_rank_length(dt, type = "one_sided_left")*9,
               c(6, 3, 2, 1, 8, 4, 7, 9, 5))
  expect_equal(extreme_rank_length(dtt, type = "one_sided_left"),
               c(0.75, 0.30, 0.20, 0.10, 0.90, 0.40, 0.60, 1.00, 0.50, 0.75))


})
