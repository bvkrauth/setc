context("is_question_data - normal")

test_that("Basic functionality", {
  expect_equal(
    is_question_data(tibble::tibble(question_id = c("BOB", "JOE"),
                                    question_text = c("Bob text", "Joe text"),
                                    table_type = c("grouped", "separate"))),
    TRUE)
  expect_equal(
    is_question_data(data.frame(question_id = c("BOB", "JOE"),
                                question_text = c("Bob text", "Joe text"),
                                table_type = c("grouped", "separate"))),
    TRUE)
  expect_equal(is_question_data(data.frame(question_id = character(0),
                                               question_text = character(0),
                                               table_type = character(0))),
               TRUE)
})

test_that("FALSE if input isn't a data frame", {
  expect_equal(is_question_data("NOT A DATA FRAME"), FALSE)
  expect_equal(is_question_data(5), FALSE)
})

test_that("FALSE if there is no question_id variable", {
  expect_equal(is_question_data(data.frame(not_question_id = c("Bob", "Joe"))),
               FALSE)
  expect_equal(is_question_data(data.frame(question_id_not = c("Bob", "Joe"))),
               FALSE)
})

test_that("FALSE if question_id has duplicates", {
  expect_equal(
    is_question_data(tibble::tibble(question_id = c("BOB", "BOB"),
                                    question_text = c("Bob text", "Joe text"),
                                    table_type = c("grouped", "separate"))),
    FALSE)
})
