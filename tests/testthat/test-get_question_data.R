context("get_question_data - normal")

test_that("Basic functionality", {
  expect_equal(nrow(get_question_data("../data/econ_question_list.csv")),
               19)
  expect_equal(nrow(get_question_data()),
               15)
  expect_equal(
    is_question_data(get_question_data("../data/econ_question_list.csv")),
    TRUE)
  expect_equal(
    is_question_data(get_question_data()),
    TRUE)
})

test_that("Fails cleanly", {
  expect_error(get_question_data("../data/file that does not exist.csv"))
  expect_error(suppressWarnings(get_question_data(
    "../data/Brian Krauth (ECON 835 (G100) - Econometrics)_Fall_2016.pdf")))
  expect_error(get_question_data("../data/econ_instructor_list.csv"))
})
