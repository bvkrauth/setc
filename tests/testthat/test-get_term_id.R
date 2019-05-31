context("term supplied")

test_that("Basic functionality", {
  expect_equal(get_term_id("Fa18"), 1187)
})

test_that("vectorized", {
  expect_equal(get_term_id(c("Fa22", "Sp23", "Su23")), c(1227, 1231, 1234))
})

test_that("Fails cleanly", {
  expect_error(get_term_id())
  expect_error(get_term_id(""))
  expect_error(get_term_id(5))
  expect_error(get_term_id("NONSENSE"))
})

context("course_id supplied")

test_that("Basic functionality", {
  expect_equal(get_term_id(course_id = "ECON381D1Fa18"), 1187)
})

test_that("Other departments", {
  expect_equal(get_term_id(course_id = "GSWS381D1Fa18"), 1187)
  expect_equal(get_term_id(course_id = "IS381D1Fa18"), 1187)
})

test_that("Fails cleanly", {
  expect_error(get_term_id(course_id = 5))
  expect_error(get_term_id(course_id = "[NON-SPECIFIC TEXT HERE]"))
})
