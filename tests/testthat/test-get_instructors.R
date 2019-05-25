test_that("Basic functionality", {
  expect_equal(nrow(get_instructors("C:/Users/Brian/sfuvault/Chair/Teaching evaluations/SETC/Data/master/master instructor list.csv")), 48)
})
