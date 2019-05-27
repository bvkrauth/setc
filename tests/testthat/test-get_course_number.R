context("Normal")

test_that("Basic functionality", {
  expect_equal(get_course_number(course_id = "ECON105D1Fa18"), 105)
  expect_equal(get_course_number("BrianKrauth -ECON105D100-Fall2018(SUP)_407d2700-5049-48d3-9ed5-465e7627b5d0en-US.pdf",
                                 term = "Fa18"),
               105)
})

test_that("Handle shorter program codes", {
#    expect_equal(get_course_number(course_id = "IS105D1Fa18"), 105)
})


test_that("Fail cleanly", {
  expect_error(get_course_number(course_id = "AAAAAAA"))
})
