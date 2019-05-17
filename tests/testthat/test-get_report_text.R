test_that("cannot test anything else yet", {
  expect_equal(2 * 2, 4)
})

test_that("Fails cleanly", {
  expect_error(get_report_text("nonexistent_file.pdf"))
})
