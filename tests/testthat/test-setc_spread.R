context("setc_spread - normal")

test_that("Basic functionality", {
  expect_equal(nrow(
    setc_spread(dat = tibble::tibble(
      course_id = c("ECON381D1Fa17","ECON381D1Fa17"),
      question_id = c("QUAL","ATND"),
      score_mean = c(3,NA)),
      stat = "mean")),
    1)
})
