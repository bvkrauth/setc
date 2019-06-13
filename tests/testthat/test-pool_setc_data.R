context("pool_setc_data - NO TESTS YET")

test_that("Fails cleanly", {
  expect_error(
    pool_setc_data(data_structure = "DATA_STRUCTURE",
                   data_folder = "DATA_FOLDER"),
    regexp = "No CSV DATA_STRUCTURE files found in folder DATA_FOLDER")
})
