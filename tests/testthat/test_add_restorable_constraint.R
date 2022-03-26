context("add_restorable_constraint")

test_that("add_restorable_constraint", {
  # Load data
  habitat <- terra::rast(system.file("extdata", "habitat_hi_res.tif", package = "restoptr"))
  # Test 1: exact amount of restoration area
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_restorable_constraint(min_restore = 90, max_restore = 90, unit = "ha", min_proportion = 1)
  result <- solve(problem)
  rest_cells <- which(result[,] == 3)
  val <- nb_cell_to_area(
    habitat,
    sum(round(get_restorable_habitat(problem))[rest_cells]),
    unit="ha"
  )
  testthat::expect_equal(round(val), set_units(90, "ha"))
  # Test 2: interval of restoration area
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_restorable_constraint(min_restore = 200, max_restore = 211, unit = "ha", min_proportion = 1)
  result <- solve(problem)
  rest_cells <- which(result[,] == 3)
  val <- nb_cell_to_area(
    habitat,
    sum(round(get_restorable_habitat(problem))[rest_cells]),
    unit="ha"
  )
  testthat::expect_gte(round(val), set_units(200, "ha"))
  testthat::expect_lte(round(val), set_units(211, "ha"))
  # Test 3: Min proportion
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_restorable_constraint(min_restore = 90, max_restore = 90, unit = "ha", min_proportion = 0.7)
  result <- solve(problem, verbose = TRUE)
  rest_cells <- which(result[,] == 3)
  cellArea <- problem$data$cell_area
  val <- nb_cell_to_area(
    habitat,
    sum(max(round(get_restorable_habitat(problem)) - ceiling(0.3 * cellArea), 0)[rest_cells]),
    unit="ha"
  )
  testthat::expect_equal(round(val), set_units(90, "ha"))
  # Test 4: Min proportion with interval
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_restorable_constraint(min_restore = 50, max_restore = 60, unit = "ha", min_proportion = 0.5)
  result <- solve(problem)
  rest_cells <- which(result[,] == 3)
  val <- nb_cell_to_area(
    habitat,
    sum(max(round(get_restorable_habitat(problem)) - ceiling(0.5 * cellArea), 0)[rest_cells]),
    unit="ha"
  )
  testthat::expect_gte(round(val), set_units(50, "ha"))
  testthat::expect_lte(round(val), set_units(60, "ha"))
})
