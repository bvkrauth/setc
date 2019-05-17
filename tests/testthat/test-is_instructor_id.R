context("Normal")

test_that("Basic functionality", {
  expect_equal(is_instructor_id("BrianKrauth"), TRUE)
  expect_equal(is_instructor_id("A"), TRUE)
#  expect_equal(is_instructor_id("Brian Krauth"), FALSE)
  expect_equal(is_instructor_id("  "), FALSE)
  expect_equal(is_instructor_id(""), FALSE)
})

test_that("Function handles vectors", {
  expect_equal(is_instructor_id(c("BrianKrauth","JoeSmith")), TRUE)
  expect_equal(is_instructor_id(c("BrianKrauth"," ")), FALSE)
})
