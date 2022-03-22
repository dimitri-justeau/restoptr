context("add_connected_constraint")

test_that("add_connected_constraint", {
  # Create problem without optimization
  habitat <- terra::rast(system.file("extdata", "habitat_hi_res.tif", package = "restoptr"))
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_connected_constraint()
  result <- solve(problem)
  if (require(landscapemetrics)) {
    np <- landscapemetrics::lsm_c_np(result, directions = 4)
    testthat::expect_equal(np[np$class == 3,]$value, 1)
  }
  # Create problem with optimization
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_connected_constraint() %>%
    set_max_mesh_objective()
  result <- solve(problem)
  if (require(landscapemetrics)) {
    np <- landscapemetrics::lsm_c_np(result, directions = 4)
    testthat::expect_equal(np[np$class == 3,]$value, 1)
  }
})
