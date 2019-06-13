context("get_setc_data - normal")

test_that("Basic functionality", {
  expect_equal(
    nrow(get_setc_data(
      report_file = "../data/BrianKrauth-ECON381D100-Spring2017.pdf",
      quietly = TRUE)$comments),
    6)
})


test_that("Fails cleanly", {
  expect_error(
    get_setc_data(),
    regexp = "Either report_file or report_folder must be provided")
  expect_error(
    get_setc_data(report_folder = "NotARealFolder"),
    regexp = "No PDF files in")
  expect_error(
    get_setc_data(report_file = "../data/nonexistent_file.pdf"),
    regexp = "File not found")
  expect_error(
    get_setc_data(report_file = "../data/econ_question_list.csv"),
    regexp = "Unable to recover a valid course ID")
})

# Currently gives bad error message when files are not
# named correctly
