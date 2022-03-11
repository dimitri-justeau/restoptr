context("add_settings")

test_that("add_settings", {
  # Create problem
  habitat <- terra::rast(system.file("extdata", "habitat.tif", package = "restoptr"))
  restorable <- terra::rast(system.file("extdata", "restorable.tif", package = "restoptr"))
  locked_out <- terra::rast(system.file("extdata", "locked-out.tif", package = "restoptr"))
  problem <- restopt_problem(habitat, restorable)
  # Check default parameters
  testthat::expect_equal(problem$settings$precision, 4L)
  testthat::expect_equal(problem$settings$time_limit, 0L)
  # Change parameters
  problem <- problem %>% add_settings(precision = 2, time_limit = 1)
  testthat::expect_equal(problem$settings$precision, 2L)
  testthat::expect_equal(problem$settings$time_limit, 1L)
  # Check that parameter are well injected to the Java solver
  problem <- problem %>%
    add_locked_out_constraint(locked_out) %>%
    add_components_constraint(min_nb_components = 1, max_nb_components = 1) %>%
    add_compactness_constraint(max_diameter = 6) %>%
    add_restorable_constraint(min_restore = 90, max_restore = 110, cell_area = 23, min_proportion = 0.7) %>%
    add_max_mesh_objective()
  result <- solve(problem)
  metadata <- attributes(result)$metadata
  # Assert that the solving time is less than 1s (more or less 10%, thus 1.1s)
  testthat::expect_true(metadata$solving.time..ms. < 1100)
  # Assert that the precision is correct
  optimal_value <- metadata$MESH_best
  testthat::expect_equal(round(optimal_value, 2), optimal_value)
})
