test_that("Basic functionality", {
  # no test yet
})

test_that("Fails cleanly", {
  expect_equal(is.na(get_response_rate(" ")), TRUE)
})
