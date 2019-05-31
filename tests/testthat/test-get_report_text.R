test_that("Basic functionality", {
  expect_equal(length(get_report_text(
    "../data/BrianKrauth-ECON381D100-Spring2017.pdf")),
    638)
})

test_that("Fails cleanly", {
  expect_error(get_report_text("nonexistent_file.pdf"))
})
