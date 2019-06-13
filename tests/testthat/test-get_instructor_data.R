context("get_instructor_data - normal")

test_that("Basic functionality", {
  expect_equal(
    nrow(get_instructor_data("../data/econ_instructor_list.csv")),
    48)
  expect_equal(
    nrow(get_instructor_data()),
    0)
  expect_true(
    is_instructor_data(get_instructor_data("../data/econ_instructor_list.csv")))
  expect_true(
    is_instructor_data(get_instructor_data()))
})

test_that("Fails cleanly", {
  expect_error(
    get_instructor_data("../data/file that does not exist.csv"),
    regexp = "File does not exist:")
  expect_error(
    suppressWarnings(get_instructor_data(
      "../data/BrianKrauth-ECON381D100-Spring2017.pdf")),
    regexp = "Invalid instructor data")
  expect_error(
    get_instructor_data("../data/econ_question_list.csv"),
    regexp = "Invalid instructor data")
})
