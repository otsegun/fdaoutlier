# test_that("hardin rocke works for n >= 100", {
#   hfn2 <- hardin_factor_numeric(1000, 2)
#   hfn3 <- hardin_factor_numeric(1000, 3)
#   hfn4 <- hardin_factor_numeric(1000, 4)
#   # dimension 2
#   expect_equal(hfn2$factor2, 5.315887967418165160893295)
#   # dimension 3
#   expect_equal(hfn3$factor2, 4.261476109014215474246611)
#   #dimension 4
#   expect_equal(hfn4$factor2, 3.69056868523171699436602466448)
#   # error
#   expect_error(hardin_factor_numeric(1000, 1),
#                regexp = "Argument \'dimension\' must be greater than or equal to 2." )
# })
