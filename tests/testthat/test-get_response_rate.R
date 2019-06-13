context("get_response_rate - normal")

test_that("Basic functionality", {
  expect_equal(get_response_rate(c("IRRELEVANT TEXT",
                                   "Responded        26",
                                   "IRRELEVANT TEXT"),
                                 row_name = "Responded"),
               26)
})

test_that("Returns NA if it can't be found", {
  expect_true(
    is.na(get_response_rate("IRRELEVANT TEXT",
                            row_name = "Responded")))
})
