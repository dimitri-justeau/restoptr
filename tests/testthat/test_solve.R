context("solve")

test_that("solve", {
  # Load data
  habitat <- terra::rast(system.file("extdata", "habitat.tif", package = "restoptr"))
  restorable <- terra::rast(system.file("extdata", "restorable.tif", package = "restoptr"))

    # Test 1: test no solution
  problem <- restopt_problem(habitat, restorable) %>%
    add_restorable_constraint(min_restore = 400, max_restore = 400, cell_area = 23, min_proportion = 1) %>%
    add_compactness_constraint(4)
  testthat::expect_error(result <- solve(problem))

  # Test 2: test no solution because of time limit
  problem <- restopt_problem(habitat, restorable) %>%
    add_restorable_constraint(min_restore = 600, max_restore = 600, cell_area = 23, min_proportion = 1) %>%
    add_compactness_constraint(7) %>%
    add_components_constraint(10, 10) %>%
    add_settings(time_limit = 1)
  testthat::expect_error(result <- solve(problem))

  # Test 3: Find optimal solution
  problem <- restopt_problem(habitat, restorable) %>%
    add_restorable_constraint(min_restore = 90, max_restore = 110, cell_area = 23, min_proportion = 1) %>%
    add_compactness_constraint(4) %>%
    add_components_constraint(1, 1) %>%
    add_max_mesh_objective()
  result <- solve(problem)
  a <- attributes(result)
  testthat::expect_true(a$metadata$optimality_proven == "true")

  # Test 4: Find best solution but not optimal (time limit) (with verobse)
  problem <- restopt_problem(habitat, restorable) %>%
    add_restorable_constraint(min_restore = 90, max_restore = 110, cell_area = 23, min_proportion = 1) %>%
    add_compactness_constraint(6) %>%
    add_max_mesh_objective() %>%
    add_settings(time_limit = 1)
  result <- solve(problem, verbose=TRUE)
  a <- attributes(result)
  testthat::expect_true(a$metadata$optimality_proven == "false")
})
