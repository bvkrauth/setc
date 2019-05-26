test_that("Basic functionality", {
  expect_equal(nrow(get_questions("../data/master question list.csv")), 19)
  expect_equal(nrow(get_questions()), 15)
#  expect_equal(is_question_data(get_questions("../data/master question list.csv")), TRUE)
#  expect_equal(is_question_data(get_questions()), TRUE)
})

test_that("Fails cleanly", {
  expect_error(get_questions("../data/file that does not exist.csv")) # file that doesn't exist
  #  expect_error(get_instructors("../data/1167 - Fall 2016/Brian Krauth (ECON 835 (G100) - Econometrics)_Fall_2016_ae41cc87-7609-4dbf-99bc-3c29f3c81e5ben-US.pdf")) # Not a CSV file
#  expect_error(get_questions("../data/master instructor list.csv"))     # not a question list
})

