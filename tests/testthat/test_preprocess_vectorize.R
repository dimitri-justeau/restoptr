context("preprocess_vectorize")

test_that("test_vectorize", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  vectorized <- preprocess_input_vect(habitat_data)
})
