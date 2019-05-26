test_that("Basic functionality", {
  expect_equal(nrow(get_instructors("../data/master instructor list.csv")), 48)
  expect_equal(nrow(get_instructors()), 0)
  expect_equal(is_instructor_data(get_instructors("../data/master instructor list.csv")), TRUE)
  expect_equal(is_instructor_data(get_instructors()), TRUE)
})


test_that("Fails cleanly", {
  expect_error(get_instructors("../data/file that does not exist.csv")) # file that doesn't exist
  #  expect_error(get_instructors("../data/1167 - Fall 2016/Brian Krauth (ECON 835 (G100) - Econometrics)_Fall_2016_ae41cc87-7609-4dbf-99bc-3c29f3c81e5ben-US.pdf")) # Not a CSV file
  expect_error(get_instructors("../data/master question list.csv"))     # not an instructor list
})
