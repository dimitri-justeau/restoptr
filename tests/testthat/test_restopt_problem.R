context("restopt_problem")

test_that("restopt_problem", {

  ## Test valid inputs
  habitat <- terra::rast(system.file("extdata", "habitat_hi_res.tif", package = "restoptr"))
  accessible <- terra::vect(system.file("extdata", "accessible_areas.gpkg", package = "restoptr"))
  locked_out <- invert_vector(accessible, extent = ext(habitat))
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_locked_out_constraint(locked_out) %>%
    add_components_constraint(min_nb_components = 1, max_nb_components = 1) %>%
    add_compactness_constraint(max_diameter = 6, unit = "cells") %>%
    add_restorable_constraint(min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 0.7) %>%
    set_max_mesh_objective()

  problem <- add_settings(problem, time_limit = 30)

  # Test print problem (just run to ensure there is no error)
  print(problem)

  # Retrieve problem data
  testthat::expect_equal(class(problem), "RestoptProblem")
  testthat::expect_equal(length(get_constraints(problem)), 4)
  testthat::expect_true(inherits(get_objective(problem), "MaxMeshObjective"))
  testthat::expect_equal(get_settings(problem)$time_limit, 30)
  testthat::expect_equal(get_aggregation_factor(problem), 16)
  testthat::expect_equal(get_habitat_threshold(problem), 0.7)
  testthat::expect_true(inherits(get_original_habitat(problem), "SpatRaster"))
  testthat::expect_true(inherits(get_existing_habitat(problem), "SpatRaster"))
  testthat::expect_true(inherits(get_restorable_habitat(problem), "SpatRaster"))
  testthat::expect_true(inherits(get_locked_out_areas(problem), "SpatRaster"))
  testthat::expect_true(inherits(get_cell_area(problem), "SpatRaster"))
  testthat::expect_equal(
    max(as.vector(get_cell_area(problem)), na.rm = TRUE),
    get_aggregation_factor(problem)^2
  )

  # Test overwrite constraint
  testthat::expect_warning(problem <- problem %>% add_compactness_constraint(5, unit = "cells"))
  i <- which(vapply(
    problem$constraints, inherits, logical(1), "CompactnessConstraint"
  ))
  testthat::expect_equal(problem$constraints[[i]]$name, "compactness (max_diameter = 5)")

  # Test overwrite objective
  testthat::expect_warning(problem <- problem %>% set_max_iic_objective())
  testthat::expect_true(inherits(problem$objective, "MaxIicObjective"))

  ## Test invalid inputs
  testthat::expect_error(restopt_problem(1))
  resized_habitat <- aggregate(habitat, 4)
  testthat::expect_error(restopt_problem(resized_habitat, restorable))
  multi_layer <- c(habitat, habitat)
  testthat::expect_error(restopt_problem(multi_layer, aggregation_factor = 16))
  testthat::expect_error(restopt_problem(habitat, aggregation_factor = -1))
  testthat::expect_error(restopt_problem(habitat, aggregation_factor = 2, habitat_threshold = 1.7))
})
