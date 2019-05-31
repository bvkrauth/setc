test_that("Basic functionality", {
  expect_equal(nrow(get_instructors("../data/econ_instructor_list.csv")),
               48)
  expect_equal(nrow(get_instructors()),
               0)
  expect_equal(
    is_instructor_data(get_instructors("../data/econ_instructor_list.csv")),
    TRUE)
  expect_equal(
    is_instructor_data(get_instructors()),
    TRUE)
})

test_that("Fails cleanly", {
  expect_error(get_instructors("../data/file that does not exist.csv"))
  expect_error(suppressWarnings(get_instructors(
    "../data/Brian Krauth (ECON 835 (G100) - Econometrics)_Fall_2016.pdf")))
  expect_error(get_instructors("../data/econ_question_list.csv"))
})
