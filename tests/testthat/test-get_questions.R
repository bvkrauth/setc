test_that("Basic functionality", {
  expect_equal(nrow(get_questions("../data/econ_question_list.csv")), 19)
  expect_equal(nrow(get_questions()), 15)
  expect_equal(is_question_data(get_questions("../data/econ_question_list.csv")), TRUE)
  expect_equal(is_question_data(get_questions()), TRUE)
})

test_that("Fails cleanly", {
  expect_error(get_questions("../data/file that does not exist.csv")) # file that doesn't exist
  expect_error(suppressWarnings(get_instructors("../data/Brian Krauth (ECON 835 (G100) - Econometrics)_Fall_2016.pdf"))) # Not a CSV file
  expect_error(get_questions("../data/econ_instructor_list.csv"))     # not a question list
})

