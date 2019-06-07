context("get_term - normal")

test_that("Basic functionality", {
  expect_equal(get_term(c("Bertille Antoine (BUEC 333 (D100) - Statistical Analysis of Economic Data)_Fall_2016_ab5297f1-1bc0-452b-b1d4-dfcb09859fa9en-US.pdf",
                          "BrianKrauth -ECON105D100-Spring2018(SUP)_407d2700-5049-48d3-9ed5-465e7627b5d0en-US.pdf")),
               c("Fa16", "Sp18"))
})

context("get_term(from = term_folder)")

test_that("from = term_folder", {
  expect_equal(get_term("1187 - Fall 2018", from = "term_folder"),
               "Fa18")
})
