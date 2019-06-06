test_that("Basic functionality", {
  expect_equal(get_response_rate(c("IRRELEVANT TEXT",
                                   "Responded        26",
                                   "IRRELEVANT TEXT"),
                                 row_name = "Responded"),
               26)
})

test_that("Fails cleanly", {
  expect_equal(is.na(get_response_rate("IRRELEVANT TEXT",
                                       row_name = "Responded")),
                     TRUE)
})
