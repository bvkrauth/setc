context("Normal")

test_that("Basic functionality", {
  expect_equal(get_course_id("BrianKrauth -ECON105D100-Fall2018(SUP)_407d2700-5049-48d3-9ed5-465e7627b5d0en-US.pdf"),
               "ECON105D1Fa18")
  expect_equal(get_course_id(c("BrianKrauth -ECON105D100-Spring2019(SUP)_407d2700-5049-48d3-9ed5-465e7627b5d0en-US.pdf",
                             "NebileBarkin -ECON103D200-Summer2017(SUP)_407d2700-5049-48d3-9ed5-465e7627b5d0en-US.pdf")),
               c("ECON105D1Sp19", "ECON103D2Su17"))
})

test_that("Handles BUEC and W courses correctly", {
  expect_equal(get_course_id("RamazanGencay -BUEC333D100-Fall2018(SUP)_96796ae1-6e24-45ff-a7e9-a91d5a1b9ddfen-US.pdf"),
               "ECON333D1Fa18")
  expect_equal(get_course_id("StephenEaston -ECON450WD100-Fall2018(SUP)_b9724c97-c2c4-4733-9f3a-a70db0fa96been-US.pdf"),
               "ECON450D1Fa18")
})

test_that("Clean failure", {
  expect_error(get_course_id("[NON-SPECIFIC TEXT HERE]"))
})

context("Fa16 term")
test_that("Basic functionality (Fa 16 term)", {
  expect_equal(get_course_id("Brian Krauth (ECON 835 (G100) - Econometrics)_Fall_2016_ae41cc87-7609-4dbf-99bc-3c29f3c81e5ben-US.pdf"),
               "ECON835G1Fa16")
})

test_that("Handles BUEC and W courses correctly (Fa16 term)", {
  expect_equal(get_course_id("Bertille Antoine (BUEC 333 (D100) - Statistical Analysis of Economic Data)_Fall_2016_ab5297f1-1bc0-452b-b1d4-dfcb09859fa9en-US.pdf"),
               "ECON333D1Fa16")
  expect_equal(get_course_id("Alexander Karaivanov (ECON 355W (D100) - Economic Development)_Fall_2016_4cc3a439-fc22-473a-b90b-4a6d3ed0b22fen-US.pdf"),
               "ECON355D1Fa16")
})

context("Non-econ")

test_that("Handles 2-character and 3-character departments", {
  expect_equal(get_course_id("BrianKrauth -BUS105D100-Fall2018(SUP)_407d2700-5049-48d3-9ed5-465e7627b5d0en-US.pdf"),
               "BUS105D1Fa18")
  expect_equal(get_course_id("BrianKrauth -IS105D100-Fall2018(SUP)_407d2700-5049-48d3-9ed5-465e7627b5d0en-US.pdf"),
               "IS105D1Fa18")
})

test_that("Handles 2-character and 3-character departments (Fa16 term)", {
  expect_equal(get_course_id("Brian Krauth (BUS 835 (G100) - Econometrics)_Fall_2016_ae41cc87-7609-4dbf-99bc-3c29f3c81e5ben-US.pdf"),
               "BUS835G1Fa16")
  expect_equal(get_course_id("Brian Krauth (IS 835 (G100) - Econometrics)_Fall_2016_ae41cc87-7609-4dbf-99bc-3c29f3c81e5ben-US.pdf"),
               "IS835G1Fa16")
})
