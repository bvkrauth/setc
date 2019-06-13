context("get_report_text - normal")

test_that("Basic functionality", {
  expect_equal(length(get_report_text(
    "../data/BrianKrauth-ECON381D100-Spring2017.pdf")),
    638)
})

test_that("Fails cleanly", {
  expect_error(
    get_report_text("nonexistent_file.pdf"),
    regexp = "File does not exist")
  expect_error(
    get_report_text("../data/econ_question_list.csv"))
})
