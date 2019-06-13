context("get_score_distribution - normal")

test_that("Basic functionality", {
  expect_equal(get_score_distribution(
    # nolint start
    report_text = c("I attended class...",
                    "       I attended class...",
                    "           Distribution of Responses",
                    "         Options                         Score Count Percentage",
                    "         All of the time                     5      17         65.4%",
                    "         Most of the time                    4        7        26.9%",
                    "         About half of the time              3        1         3.8%",
                    "         Rarely                              2        1         3.8%",
                    "         Never                               1        0         0.0%"),
    # nolint end
    question = tibble::tibble(question_id = "ATND",
                              question_text = "I attended class")),
               c(0, 1, 1, 7, 17))
  })


test_that("Returns NA if it can't be found", {
  expect_true(
    all(is.na(
      get_score_distribution(
      # nolint start
      report_text = c("I attended class...",
                      "       I attended class...",
                      "           Distribution of Responses",
                      "         Options                         Score Count Percentage",
                      "         All of the time                     5      17         65.4%",
                      "         Most of the time                    4        7        26.9%",
                      "         About half of the time              3        1         3.8%",
                      "         Rarely                              2        1         3.8%",
                      "         Never                               1        0         0.0%"),
      # nolint end
      question = tibble::tibble(question_id = "ATND",
                                question_text = "TEXT THAT DOESNT MATCH")
      ))))
})
