context("Normal")

test_that("Basic functionality", {
  expect_equal(get_course_id("BrianKrauth -ECON105D100-Fall2018(SUP)_407d2700-5049-48d3-9ed5-465e7627b5d0en-US.pdf", term = "Fa18"), "ECON105D1Fa18")
})

test_that("Handles BUEC and W courses correctly", {
  expect_equal(get_course_id("RamazanGencay -BUEC333D100-Fall2018(SUP)_96796ae1-6e24-45ff-a7e9-a91d5a1b9ddfen-US.pdf", term = "Fa18"), "ECON333D1Fa18")
  expect_equal(get_course_id("StephenEaston -ECON450WD100-Fall2018(SUP)_b9724c97-c2c4-4733-9f3a-a70db0fa96been-US.pdf", term = "Fa18"), "ECON450D1Fa18")
})

test_that("Clean failure", {
  expect_error(get_course_id("BrianKrauth -ECON105D100-Fall2018(SUP)_407d2700-5049-48d3-9ed5-465e7627b5d0en-US.pdf"))
  expect_error(get_course_id("A", term = "Fa18"))
})

context("Fa16 term")
test_that("Basic functionality (Fa 16 term)", {
  expect_equal(get_course_id("Brian Krauth (ECON 835 (G100) - Econometrics)_Fall_2016_ae41cc87-7609-4dbf-99bc-3c29f3c81e5ben-US.pdf", term = "Fa16"), "ECON835G1Fa16")
})

test_that("Handles BUEC and W courses correctly (Fa16 term)", {
  expect_equal(get_course_id("Bertille Antoine (BUEC 333 (D100) - Statistical Analysis of Economic Data)_Fall_2016_ab5297f1-1bc0-452b-b1d4-dfcb09859fa9en-US.pdf", term = "Fa16"), "ECON333D1Fa16")
  expect_equal(get_course_id("Alexander Karaivanov (ECON 355W (D100) - Economic Development)_Fall_2016_4cc3a439-fc22-473a-b90b-4a6d3ed0b22fen-US.pdf", term = "Fa16"), "ECON355D1Fa16")
})
