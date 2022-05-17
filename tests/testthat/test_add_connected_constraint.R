context("add_connected_constraint")

test_that("expected result", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_connected_constraint() %>%
    add_settings(time_limit = 30)
  result <- solve(problem, verbose = TRUE)
  # tests
  expect_is(result, "SpatRaster")
  skip_if_not_installed("landscapemetrics")
  n_components <- landscapemetrics::lsm_c_np(result, directions = 4)
  expect_equal(n_components$value[n_components$class == 3], 1)
})
