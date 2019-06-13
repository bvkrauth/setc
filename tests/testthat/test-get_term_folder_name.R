context("get_term_folder_name - normal")

test_that("basic functionality", {
  expect_equal(
    get_term_folder_name(c("Fa17", "Sp18", "Su18")),
    c("1177 - Fall 2017",
      "1181 - Spring 2018",
      "1184 - Summer 2018"))
})


test_that("Fails cleanly", {
  expect_error(
    get_term_folder_name(),
    regexp = "is missing, with no default")
  expect_error(
    get_term_folder_name(""),
    regexp = "Invalid term")
  expect_error(
    get_term_folder_name(5),
    regexp = "Invalid term")
  expect_error(
    get_term_folder_name("NONSENSE"),
    regexp = "Invalid term")
})
