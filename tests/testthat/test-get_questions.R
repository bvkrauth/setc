test_that("Basic functionality", {
  expect_equal(nrow(get_questions("C:/Users/Brian/sfuvault/Chair/Teaching evaluations/SETC/Data/master/master question list.csv")), 19)
  expect_equal(nrow(get_questions()), 15)
})

