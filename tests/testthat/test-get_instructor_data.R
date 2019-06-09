context("get_instructor_data - normal")

test_that("Basic functionality", {
  expect_equal(nrow(get_instructor_data("../data/econ_instructor_list.csv")),
               48)
  expect_equal(nrow(get_instructor_data()),
               0)
  expect_equal(
    is_instructor_data(get_instructor_data("../data/econ_instructor_list.csv")),
    TRUE)
  expect_equal(
    is_instructor_data(get_instructor_data()),
    TRUE)
})

test_that("Fails cleanly", {
  expect_error(get_instructor_data("../data/file that does not exist.csv"))
  expect_error(suppressWarnings(get_instructor_data(
    "../data/Brian Krauth (ECON 835 (G100) - Econometrics)_Fall_2016.pdf")))
  expect_error(get_instructor_data("../data/econ_question_list.csv"))
})
