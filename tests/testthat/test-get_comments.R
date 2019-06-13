context("get_comments - normal")

test_that("Returns comments if there are comments", {
  expect_equal(
    nrow(get_comments(c("Do you have any further comments?",
                        "Skip first line",
                        " First comment",
                        "Skip lines that don't start with whitespace",
                        " Second comment"),
                      course_id = "ECON381D1Fa18")),
    2)
})

test_that("Returns NULL if no comments found", {
  expect_equal(
    get_comments("Do you have any further comments?"),
    NULL)
  expect_equal(
    get_comments("NO COMMENTS HERE"),
    NULL)
})
