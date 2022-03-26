context("solve")

test_that("solve", {
  # Load data
  habitat <- terra::rast(system.file("extdata", "habitat_hi_res.tif", package = "restoptr"))
  # Test 1: test no solution
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_restorable_constraint(min_restore = 400, max_restore = 400, unit = "ha", min_proportion = 1) %>%
    add_compactness_constraint(1800, unit = "m")
  testthat::expect_error(result <- solve(problem))

  # Test 2: test no solution because of time limit
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_restorable_constraint(min_restore = 600, max_restore = 600, unit = "ha", min_proportion = 1) %>%
    add_compactness_constraint(3.1, unit = "km") %>%
    add_components_constraint(10, 10) %>%
    add_settings(time_limit = 1)
  testthat::expect_error(result <- solve(problem))

  # Test 3: Find optimal solution
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_restorable_constraint(min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 1) %>%
    add_compactness_constraint(4, unit = "cells") %>%
    add_components_constraint(1, 1) %>%
    set_max_mesh_objective()
  result <- solve(problem)
  a <- attributes(result)
  testthat::expect_true(a$metadata$optimality_proven == "true")

  # Test 4: Find best solution but not optimal (time limit) (with verobse)
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_restorable_constraint(min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 1) %>%
    add_compactness_constraint(6, unit = "cells") %>%
    set_max_mesh_objective() %>%
    add_settings(time_limit = 1)
  result <- solve(problem, verbose=TRUE)
  a <- attributes(result)
  testthat::expect_true(a$metadata$optimality_proven == "false")
})
