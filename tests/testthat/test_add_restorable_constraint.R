context("restopt_problem")

test_that("add_restorable_constraint", {
  # Test 1: exact amount of restoration area
  habitat <- terra::rast(system.file("extdata", "habitat.tif", package = "restoptr"))
  restorable <- terra::rast(system.file("extdata", "restorable.tif", package = "restoptr"))
  problem <- restopt_problem(habitat, restorable) %>%
    add_restorable_constraint(min_restore = 90, max_restore = 90, cell_area = 23, min_proportion = 1)
  result <- solve(problem, verbose=TRUE)
  rest_cells <- which(result[,] == 2)
  val <- sum(round(restorable)[rest_cells])
  testthat::expect_equal(val, 90)
  # Test 2: interval of restoration area
  problem <- restopt_problem(habitat, restorable) %>%
    add_restorable_constraint(min_restore = 200, max_restore = 211, cell_area = 23, min_proportion = 1)
  result <- solve(problem, verbose=TRUE)
  rest_cells <- which(result[,] == 2)
  val <- sum(round(restorable)[rest_cells])
  testthat::expect_gte(val, 200)
  testthat::expect_lte(val, 211)
  # Test 3: Min proportion
  problem <- restopt_problem(habitat, restorable) %>%
    add_restorable_constraint(min_restore = 90, max_restore = 90, cell_area = 23, min_proportion = 0.7)
  result <- solve(problem)
  rest_cells <- which(result[,] == 2)
  val <- sum(max(round(restorable) - ceiling(0.3 * 23), 0)[rest_cells])
  testthat::expect_equal(val, 90)
  # Test 4: Min proportion with interval
  problem <- restopt_problem(habitat, restorable) %>%
    add_restorable_constraint(min_restore = 50, max_restore = 60, cell_area = 23, min_proportion = 0.5)
  result <- solve(problem)
  rest_cells <- which(result[,] == 2)
  val <- sum(max(round(restorable) - ceiling(0.5 * 23), 0)[rest_cells])
  testthat::expect_gte(val, 50)
  testthat::expect_lte(val, 60)
})
