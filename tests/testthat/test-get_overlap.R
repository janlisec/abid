testthat::test_that("get_overlap works", {
  x <- data.frame("m"=c(1,2.1,2.09,3.001), "i"=rep(1,4))
  y <- data.frame("m"=1:3, "i"=rep(1,3))
  
  # the allowed deviation `dmz` will define the number of peaks in x having a match in y
  testthat::expect_equal(ABID:::get_overlap(x=x, y=y, type="sum"), 4L)
  testthat::expect_equal(ABID:::get_overlap(x=x, y=y, type="sum", dmz=0.1), 3L)
  testthat::expect_equal(ABID:::get_overlap(x=x, y=y, type="sum", dmz=0.01), 2L)
  testthat::expect_equal(ABID:::get_overlap(x=x, y=y, type="sum", dmz=0), 1L)
  
  # alternative outputs will provide the relative intensity or
  # mass values of matching peaks from the spectrum
  testthat::expect_equal(ABID:::get_overlap(x=x, y=y, type="rel", dmz=0), 0.25)
  testthat::expect_equal(ABID:::get_overlap(x=x, y=y, type="masses", dmz=0.01), x[c(1,4),1])
  testthat::expect_equal(ABID:::get_overlap(x=x, y=y, type="intweight", dmz=0.01), 0.5)
})
