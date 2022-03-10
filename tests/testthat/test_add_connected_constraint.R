context("restopt_problem")

test_that("add_components_constraint", {
  # Create problem without optimization
  habitat <- terra::rast(system.file("extdata", "habitat.tif", package = "restoptr"))
  restorable <- terra::rast(system.file("extdata", "restorable.tif", package = "restoptr"))
  problem <- restopt_problem(habitat, restorable) %>%
    add_connected_constraint()
  result <- solve(problem)
  np <- landscapemetrics::lsm_c_np(result, directions = 4)
  testthat::expect_equal(np[np$class == 2,]$value, 1)
  # Create problem with optimization
  habitat <- terra::rast(system.file("extdata", "habitat.tif", package = "restoptr"))
  restorable <- terra::rast(system.file("extdata", "restorable.tif", package = "restoptr"))
  problem <- restopt_problem(habitat, restorable) %>%
    add_connected_constraint() %>%
    add_max_mesh_objective()
  result <- solve(problem)
  np <- landscapemetrics::lsm_c_np(result, directions = 4)
  testthat::expect_equal(np[np$class == 2,]$value, 1)
})
