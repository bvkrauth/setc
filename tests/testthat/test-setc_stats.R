context("Normal")

test_that("basic functionality", {
  expect_equal(setc_n(tibble::tibble(score_1 = 1,
                                     score_2 = 1,
                                     score_3 = 1,
                                     score_4 = 1,
                                     score_5 = 1)),
               5)
  expect_equal(setc_mean(tibble::tibble(score_1 = 1,
                                        score_2 = 1,
                                        score_3 = 1,
                                        score_4 = 1,
                                        score_5 = 1)),
               3)
  expect_equal(setc_median(tibble::tibble(score_1 = 1,
                                          score_2 = 1,
                                          score_3 = 1,
                                          score_4 = 1,
                                          score_5 = 1)),
               3)
  expect_lt(abs(setc_sd(tibble::tibble(score_1 = 1,
                                       score_2 = 1,
                                       score_3 = 1,
                                       score_4 = 1,
                                       score_5 = 1)) -
                  1.58),
            0.01)
})

test_that("no observations", {
  expect_equal(setc_n(tibble::tibble(score_1 = 0,
                                     score_2 = 0,
                                     score_3 = 0,
                                     score_4 = 0,
                                     score_5 = 0)),
               0)
  expect_equal(is.na(setc_median(tibble::tibble(score_1 = 0,
                                                score_2 = 0,
                                                score_3 = 0,
                                                score_4 = 0,
                                                score_5 = 0))),
               TRUE)
  expect_equal(is.na(setc_mean(tibble::tibble(score_1 = 0,
                                              score_2 = 0,
                                              score_3 = 0,
                                              score_4 = 0,
                                              score_5 = 0))),
               TRUE)
  expect_equal(is.na(setc_sd(tibble::tibble(score_1 = 0,
                                            score_2 = 0,
                                            score_3 = 0,
                                            score_4 = 0,
                                            score_5 = 0))),
               TRUE)
})






test_that("distribution supressed (NA)", {
  expect_equal(is.na(setc_n(tibble::tibble(score_1 = NA,
                                           score_2 = NA,
                                           score_3 = NA,
                                           score_4 = NA,
                                           score_5 = NA))),
               TRUE)
  expect_equal(is.na(setc_mean(tibble::tibble(score_1 = NA,
                                              score_2 = NA,
                                              score_3 = NA,
                                              score_4 = NA,
                                              score_5 = NA))),
               TRUE)
  expect_equal(is.na(setc_median(tibble::tibble(score_1 = NA,
                                                score_2 = NA,
                                                score_3 = NA,
                                                score_4 = NA,
                                                score_5 = NA))),
               TRUE)
  expect_equal(is.na(setc_sd(tibble::tibble(score_1 = NA,
                                            score_2 = NA,
                                            score_3 = NA,
                                            score_4 = NA,
                                            score_5 = NA))),
               TRUE)
})
