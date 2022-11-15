testthat::test_that("GetGroupFactor works", {
  testthat::expect_error(ABID:::GetGroupFactor(x = "a"))
  testthat::expect_error(ABID:::GetGroupFactor(x = 1:10))
  testthat::expect_true(is.factor(ABID:::GetGroupFactor(x = 1:3, gap = 1)))
  testthat::expect_equal(length(ABID:::GetGroupFactor(x = 1:3, gap = 1)), 3L)
  testthat::expect_equal(length(levels(ABID:::GetGroupFactor(x = c(1:3, 5), gap = 1))), 2L)
})
