context("Default settings (strict = FALSE)")
test_that("function is case insensitive if strict = FALSE (default)", {
  expect_equal(is_course_id("ECON381D1Fa16"), TRUE)
  expect_equal(is_course_id("econ381D1Fa16"), TRUE)
  expect_equal(is_course_id("ECON381d1Fa16"), TRUE)
  expect_equal(is_course_id("ECON381D1fA16"), TRUE)
})

test_that("function allows other units", {
  expect_equal(is_course_id("IS381D1Fa16"), TRUE)
  expect_equal(is_course_id("GSWS381D1Fa16"), TRUE)
})

test_that("function allows any section type", {
  expect_equal(is_course_id("ECON381A1Fa16"), TRUE)
  expect_equal(is_course_id("ECON381C1Fa16"), TRUE)
  expect_equal(is_course_id("ECON381D1Fa16"), TRUE)
  expect_equal(is_course_id("ECON381E1Fa16"), TRUE)
  expect_equal(is_course_id("ECON381G1Fa16"), TRUE)
})

test_that("function allows all valid terms", {
  expect_equal(is_course_id("ECON381D1AA16"), FALSE)
  expect_equal(is_course_id("ECON381D1Fa16"), TRUE)
  expect_equal(is_course_id("ECON381D1Sp16"), TRUE)
  expect_equal(is_course_id("ECON381D1Su16"), TRUE)
})

test_that("spaces not allowed", {
  expect_equal(is_course_id("BUS 381D1Fa16"), FALSE)
  expect_equal(is_course_id("ECON381 D1Fa16"), FALSE)
})

test_that("function does not allow extra length", {
  expect_equal(is_course_id("extraECON381D1Fa16"), FALSE)
  expect_equal(is_course_id("ECON381D1Fa16extra"), FALSE)
})

test_that("function handles vectors", {
  expect_equal(is_course_id(c("ECON381D1Fa16", "ECON105G1Sp19")), TRUE)
  expect_equal(is_course_id(c("ECON381D1Fa16", "A")), FALSE)
})

context("When strict = TRUE")

test_that("function is case sensitive when strict=TRUE", {
  expect_equal(is_course_id("ECON381D1Fa16", strict = TRUE), TRUE)
  expect_equal(is_course_id("econ381D1Fa16", strict = TRUE), FALSE)
  expect_equal(is_course_id("ECON381d1Fa16", strict = TRUE), FALSE)
  expect_equal(is_course_id("ECON381D1fA16", strict = TRUE), FALSE)
})

test_that("function allows only known section types if strict = TRUE", {
  expect_equal(is_course_id("ECON381A1Fa16", strict = TRUE), FALSE)
  expect_equal(is_course_id("ECON381C1Fa16", strict = TRUE), TRUE)
  expect_equal(is_course_id("ECON381D1Fa16", strict = TRUE), TRUE)
  expect_equal(is_course_id("ECON381E1Fa16", strict = TRUE), TRUE)
  expect_equal(is_course_id("ECON381G1Fa16", strict = TRUE), TRUE)
})
