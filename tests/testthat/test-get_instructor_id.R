context("get_instructor_id - normal")

test_that("Basic functionality", {
  expect_equal(
    get_instructor_id(
      "BrianKrauth -ECON105D100-Fall2018(SUP)_407d2700-5049-48d3-9ed5-465e7627b5d0en-US.pdf"), # nolint
    "BrianKrauth")
  expect_equal(get_instructor_id(c(
    "BrianKrauth -ECON105D100-Fall2018(SUP)_407d2700-5049-48d3-9ed5-465e7627b5d0en-US.pdf", # nolint
    "Brian Krauth (ECON 835 (G100) - Econometrics)_Fall_2016_ae41cc87-7609-4dbf-99bc-3c29f3c81e5ben-US.pdf")), # nolint
    c("BrianKrauth", "BrianKrauth"))
})

test_that("Clean failure", {
  expect_error(
    get_instructor_id(""),
    regexp = "Unable to recover a valid instructor ID")
})


context("get_instructor_id (Fa16 term)")
test_that("Basic functionality (Fa 16 term)", {
  expect_equal(
    get_instructor_id(
      "Brian Krauth (ECON 835 (G100) - Econometrics)_Fall_2016_ae41cc87-7609-4dbf-99bc-3c29f3c81e5ben-US.pdf"),  # nolint
    "BrianKrauth")
})

context("get_instructor_id (non-ECON)")

test_that("Basic functionality", {
  expect_equal(
    get_instructor_id(
      "BrianKrauth -BUS105D100-Fall2018(SUP)_407d2700-5049-48d3-9ed5-465e7627b5d0en-US.pdf"), # nolint
    "BrianKrauth")
  expect_equal(
    get_instructor_id(
      "BrianKrauth -IS105D100-Fall2018(SUP)_407d2700-5049-48d3-9ed5-465e7627b5d0en-US.pdf"), # nolint
    "BrianKrauth")
})

test_that("Basic functionality (Fa 16 term)", {
  expect_equal(get_instructor_id(
    "Brian Krauth (BUS 835 (G100) - Econometrics)_Fall_2016_ae41cc87-7609-4dbf-99bc-3c29f3c81e5ben-US.pdf"), # nolint
    "BrianKrauth")
})
