context("get_comments - normal")

test_that("Returns comments if there are comments", {
  expect_equal(
    nrow(get_comments(c("Do you have any further comments?",
                        rep("    Hello", times = 3)),
                      course_id = "ECON381D1Fa18")),
    2)
})

test_that("Returns NULL if no comments found", {
  expect_equal(
    get_comments("Do you have any further comments?"),
    NULL)
  expect_equal(
    get_comments("NON-SPECIFIC TEXT HERE"),
    NULL)
})
