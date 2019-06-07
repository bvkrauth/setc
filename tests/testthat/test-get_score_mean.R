context("get_score_mean(dat$table_type = separate)")

test_that("Finds correct mean", {
  expect_equal(get_score_mean(report_text = c("I attended class...",
                                 "Scale used: 1=Never, 2=Rarely, 3=About half of the time, 4=Most of the time, 5=All of the time",
                                  "         Mean                                                    SD                                      Resp",
                                 "         4.54                                                    0.76                                    26"),
                 question = tibble::tibble(question_id = "ATND",
                                       question_text = "I attended class",
                                       table_type = "separate")),
               4.54)
})


test_that("Deals with missing question", {
  expect_equal(get_score_mean(report_text = c("I attended class...",
                                              "Scale used: 1=Never, 2=Rarely, 3=About half of the time, 4=Most of the time, 5=All of the time",
                                              "         Mean                                                    SD                                      Resp",
                                              "         4.54                                                    0.76                                    26"),
                              question = tibble::tibble(question_id = "ATND",
                                                        question_text = "IRRELEVANT TEXT",
                                                        table_type = "separate")),
               NA)
})


context("get_score_mean(dat$table_type = grouped)")

test_that("Finds correct mean", {
  expect_equal(get_score_mean(report_text = c("                                                                                                              Mean SD   Resp",
                                              "         The course instructor explained course concepts clearly.                                             4.28 0.74 25",
                                              "         The course instructor explained grading criteria clearly.                                            4.52 0.59 25",
                                              "         The course instructor created a respectful learning environment.                                     4.40 0.71 25",
                                              "         The course instructor was approachable when students asked for guidance.                             4.42 0.72 24"),
                              question = tibble::tibble(question_id = "ENVI",
                                                        question_text = "The course instructor created a respectful learning environment",
                                                        table_type = "grouped")),
               4.40)
})

test_that("Finds correct mean", {
  expect_equal(get_score_mean(report_text = c("                                                                                                              Mean SD   Resp",
                                              "         The course instructor explained course concepts clearly.                                             4.28 0.74 25",
                                              "         The course instructor explained grading criteria clearly.                                            4.52 0.59 25",
                                              "         The course instructor created a respectful learning environment.                                     4.40 0.71 25",
                                              "         The course instructor was approachable when students asked for guidance.                             4.42 0.72 24"),
                              question = tibble::tibble(question_id = "ENVI",
                                                        question_text = "IRRELEVANT TEXT",
                                                        table_type = "grouped")),
               NA)
})
