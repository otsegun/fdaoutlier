test_that("sequential transformation handles unconventional data", {
  expect_error(seq_transform(dts = list()))
  expect_error(seq_transform(dts = data.frame()) )
  expect_error(seq_transform(dts = matrix(0, nrow = 1, ncol = 3)))
  expect_error(seq_transform(dts = matrix(NA, nrow = 1, ncol = 3)))
  expect_error(seq_transform(dts = matrix(c(1:2, NA), nrow = 1, ncol = 3)))
})
dt1 <- simulation_model1(seed = 50)
test_that("sequential transformation fails gracefully when transformation is unknown", {

  expect_error(seq_transform(dts = dt1$data, sequence = "TO"),
               "Transformation TO not supported")
})


test_that("sequential transformation T0 and D0 works", {
  res1 <- functional_boxplot(dt = dt1$data, depth_method = "dirout")
  res1q <- seq_transform(dts = dt1$data,
                         sequence = "T0", depth_method = "dirout",
                         save_data = T)
  res2q <- seq_transform(dts = dt1$data,
                         sequence = "D0", depth_method = "dirout",
                         save_data = F)
  expect_identical(res1$outliers, res2q$outliers$D0)
  expect_identical(res1$outliers, res1q$outliers$T0)

})

test_that("sequential transformation T1 works", {
  akin <- dt1$data - rowMeans(dt1$data)
  res1 <- functional_boxplot(dt = akin, depth_method = "mbd")
  res1q <- seq_transform(dts = dt1$data,
                         sequence = "T1",
                         depth_method = "mbd",
                         save_data = T)
  expect_identical(res1$outliers, res1q$outliers$T1)
  expect_identical(akin, res1q$transformed_data$T1)
})

test_that("sequential transformation T2 works", {
  akin <- dt1$data/sqrt(rowSums(dt1$data^2))
  res1 <- functional_boxplot(dt = akin,
                             depth_method = "mbd")
  res1q <- seq_transform(dts = dt1$data,
                         sequence = "T2",
                         depth_method = "mbd",
                         save_data = T)
  expect_identical(res1$outliers, res1q$outliers$T2)
  expect_identical(akin, res1q$transformed_data$T2)
})

test_that("sequential transformation D1 and D2 works independently", {
  p <- dim(dt1$data)[2]
  akin <-   dt1$data[,2:p] - dt1$data[, 1:(p-1)]
  res1 <- functional_boxplot(dt = akin,
                                      depth_method = "mbd")
  res1q <- seq_transform(dts = dt1$data,
                         sequence = "D1",
                         depth_method = "mbd",
                         save_data = T)
  res2q <- seq_transform(dts = dt1$data,
                         sequence = "D2",
                         depth_method = "mbd",
                         save_data = T)
  expect_identical(res1$outliers, res1q$outliers$D1)
  expect_identical(res1$outliers, res2q$outliers$D2)
  expect_identical(akin, res1q$transformed_data$D1)
  expect_identical(akin, res2q$transformed_data$D2)
})

test_that("sequential transformation D1 and D2 works combined", {
  p <- dim(dt1$data)[2]
  akin <-   dt1$data[,2:p] - dt1$data[, 1:(p-1)]
  p2 <- dim(akin)[2]
  akin2 <- akin[,2:p2] - akin[, 1:(p2-1)]
  res1 <- functional_boxplot(dt = akin2,
                                      depth_method = "mbd")
  res1q <- seq_transform(dts = dt1$data,
                         sequence = c("D1", "D2"),
                         depth_method = "mbd",
                         save_data = T)
  expect_warning(res2q <- seq_transform(dts = dt1$data,
                         sequence = c("D1","D1"),
                         depth_method = "mbd",
                         save_data = T))

  expect_identical(res1$outliers, unlist(res1q$outliers))
  expect_identical(res1$outliers, unlist(res2q$outliers))
  expect_identical(akin2, res1q$transformed_data$D2)
  expect_identical(akin, res1q$transformed_data$D1)
  expect_identical(akin2, res2q$transformed_data$D1_2)
  expect_identical(akin, res2q$transformed_data$D1_1)
})


test_that("sequential transformation O works", {
  dtm <- array(0, dim = c(100, 50, 2))
  dtm[,,1] <- dt1$data
  dtm[,,2] <- dt1$data
  seqobj <- seq_transform(dtm, sequence = "O", depth_method = "erld",
                          erld_type = "one_sided_right", save_data = TRUE)

  expect_identical(seqobj$outliers$O,
                   c(14L, 20L, 36L, 43L, 53L, 70L, 96L))

  outt <- apply(dtm, 2, function(x){
    (1/projection_depth(x, n_projections = 200L, seed = 123)) - 1
  })
  seqobj2 <- functional_boxplot(outt, depth_method = "erld",
                                erld_type = "one_sided_right")
  expect_identical(seqobj2$outliers, seqobj$outliers$O)

})

test_that("sequential transformation O works with univariate data", {

  seqobj <- seq_transform(dt1$data, sequence = "O", depth_method = "erld",
                          erld_type = "one_sided_right", save_data = TRUE)

  expect_identical(seqobj$outliers$O,
                   c(14L, 20L, 36L, 43L, 53L, 70L, 96L))

})
