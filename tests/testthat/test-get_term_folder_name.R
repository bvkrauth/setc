test_that("basic functionality", {
  expect_equal(get_term_folder_name("Fa17"), "1177 - Fall 2017")
})

test_that("not actually vectorized", {
#  expect_equal(get_term_folder_name(c("Fa17", "Sp18", "Su18")),
#               c("1177 - Fall 2017",
#                 "1181 - Spring 2018",
#                 "1184 - Summer 2018"))
})


test_that("Fails cleanly", {
  expect_error(get_term_folder_name())
  expect_error(get_term_folder_name(""))
  expect_error(get_term_folder_name(5))
  expect_error(get_term_folder_name("NONSENSE"))
})
