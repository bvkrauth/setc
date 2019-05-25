test_that("Basic functionality", {
  expect_equal(get_term("Bertille Antoine (BUEC 333 (D100) - Statistical Analysis of Economic Data)_Fall_2016_ab5297f1-1bc0-452b-b1d4-dfcb09859fa9en-US.pdf"), "Fa16")
  expect_equal(get_term("BrianKrauth -ECON105D100-Fall2018(SUP)_407d2700-5049-48d3-9ed5-465e7627b5d0en-US.pdf"), "Fa18")
})

test_that("from = term_folder", {
  expect_equal(get_term("1187 - Fall 2018", from = "term_folder"), "Fa18")
})
