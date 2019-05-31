test_that("Basic functionality", {
  expect_equal(
    is_instructor_data(tibble::tibble(instructor_id = c("Bob", "Joe"))),
    TRUE)
  expect_equal(
    is_instructor_data(data.frame(instructor_id = c("Bob", "Joe"))),
    TRUE)
  expect_equal(
    is_instructor_data(data.frame(instructor_id = character(0))),
    TRUE)
})

test_that("FALSE if input isn't a data frame", {
  expect_equal(is_instructor_data("NOT A DATA FRAME"), FALSE)
  expect_equal(is_instructor_data(5), FALSE)
})

test_that("FALSE if there is no instructor_id variable", {
  expect_equal(is_instructor_data(data.frame(not_instructor_id = c("Bob",
                                                                   "Joe"))),
               FALSE)
  expect_equal(is_instructor_data(data.frame(instructor_id_not = c("Bob",
                                                                   "Joe"))),
               FALSE)
})

test_that("FALSE if instructor_id has duplicates", {
  expect_equal(is_instructor_data(data.frame(instructor_id = c("Bob",
                                                               "Bob",
                                                               "Joe"))),
               FALSE)
})
