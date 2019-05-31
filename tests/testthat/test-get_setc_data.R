test_that("Basic functionality", {
  expect_equal(
    nrow(get_setc_data(
      report_file = "../data/BrianKrauth-ECON381D100-Spring2017.pdf")$comments),
    6)
})
