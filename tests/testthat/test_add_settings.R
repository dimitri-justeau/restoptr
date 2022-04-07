context("add_settings")

test_that("add_settings", {
  # Create problem
  habitat <- terra::rast(system.file("extdata", "habitat_hi_res.tif", package = "restoptr"))
  accessible <- terra::vect(system.file("extdata", "accessible_areas.gpkg", package = "restoptr"))
  locked_out <- invert_vector(accessible, extent = ext(habitat))
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7)
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
    add_compactness_constraint(max_diameter = 6, unit = "cells") %>%
    add_restorable_constraint(min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 0.7) %>%
    set_max_mesh_objective()
  result <- solve(problem)
  metadata <- get_metadata(result, area_unit = "cells")
  # Assert that the solving time is less than 1s (more or less 10%, thus 1.1s)
  testthat::expect_true(metadata$solving_time < 1.1)
  # Assert that the precision is correct
  optimal_value <- metadata$mesh_best
  testthat::expect_equal(round(optimal_value, 2), optimal_value)
})
