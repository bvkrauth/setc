context("get_question_data - normal")

test_that("Basic functionality", {
  expect_equal(
    nrow(get_question_data("../data/econ_question_list.csv")),
    19)
  expect_equal(
    nrow(get_question_data()),
    15)
  expect_true(
    is_question_data(get_question_data("../data/econ_question_list.csv")))
  expect_true(
    is_question_data(get_question_data()))
})

test_that("Fails cleanly", {
  expect_error(
    get_question_data("../data/file that does not exist.csv"),
    regexp = "File does not exist:")
  expect_error(
    suppressWarnings(get_question_data(
      "../data/BrianKrauth-ECON381D100-Spring2017.pdf")),
    regexp = "Invalid question data")
  expect_error(
    get_question_data("../data/econ_instructor_list.csv"),
    regexp = "Invalid question data")
})
