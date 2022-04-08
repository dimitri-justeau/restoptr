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
  a <- get_metadata(result)
  testthat::expect_true(a$optimality_proven == "true")

  # Test 4: Find best solution but not optimal (time limit) (with verbose)
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_restorable_constraint(min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 1) %>%
    add_compactness_constraint(6, unit = "cells") %>%
    set_max_mesh_objective() %>%
    add_settings(time_limit = 1)
  result <- solve(problem, verbose=TRUE)
  a <- get_metadata(result)
  testthat::expect_true(a$optimality_proven == "false")

  # Test 5: Find 10 solutions without optimization
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_restorable_constraint(min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 1) %>%
    add_compactness_constraint(6, unit = "cells") %>%
    set_no_objective() %>%
    add_settings(time_limit = 1, nb_solutions = 10)
  result <- solve(problem)
  testthat::expect_true(is.list(result))
  testthat::expect_length(result, 10)
  for (i in seq(1, 10)) {
    r <- result[[i]]
    testthat::expect_true(inherits(r, "RestoptSolution"))
    testthat::expect_equal(names(r), paste("Solution", i))
  }

  # Test 6: Find 10 solutions with MESH optimization - but not enough time to enumerate
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_restorable_constraint(min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 1) %>%
    add_compactness_constraint(6, unit = "cells") %>%
    set_max_mesh_objective() %>%
    add_settings(time_limit = 5, nb_solutions = 10)
  result <- solve(problem)
  testthat::expect_length(result, 1)

  # Test 7: Find 10 solutions with MESH optimization - but only one optimal soluion exists
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    set_max_mesh_objective() %>%
    add_settings(time_limit = 10, nb_solutions = 10)
  result <- solve(problem)
  testthat::expect_length(result, 1)

  # Test 8: Find 10 solutions with IIC optimization
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_restorable_constraint(min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 1) %>%
    add_compactness_constraint(4, unit = "cells") %>%
    set_max_iic_objective() %>%
    add_settings(time_limit = 10, nb_solutions = 10)
  result <- solve(problem)
  testthat::expect_length(result, 10)
  for (i in seq(1, 10)) {
    r <- result[[i]]
    testthat::expect_true(inherits(r, "RestoptSolution"))
    testthat::expect_equal(names(r), paste("Solution", i))
    a <- get_metadata(r)
    testthat::expect_true(a$optimality_proven == "true")
  }

  # Test 9: Ask for 100 solutions without enough time
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_restorable_constraint(min_restore = 109, max_restore = 110, unit = "ha", min_proportion = 0.7) %>%
    add_compactness_constraint(3, unit = "cells") %>%
    set_no_objective() %>%
    add_settings(time_limit = 1, nb_solutions = 100)
  result <- solve(problem)
  testthat::expect_true(length(result) < 100)
})
