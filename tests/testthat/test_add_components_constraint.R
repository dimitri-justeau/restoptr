context("add_components_constraint")

test_that("add_components_constraint", {
  # Create problem with 1 component
  habitat <- terra::rast(system.file("extdata", "habitat_hi_res.tif", package = "restoptr"))
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_components_constraint(min_nb_components = 1, max_nb_components = 1)
  result <- solve(problem)
  if (require(landscapemetrics)) {
    np <- landscapemetrics::lsm_c_np(result, directions = 4)
    testthat::expect_equal(np[np$class == 3,]$value, 1)
  }
  # Create problem with 2 components
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_components_constraint(min_nb_components = 2, max_nb_components = 2)
  result <- solve(problem)
  if (require(landscapemetrics)) {
    np <- landscapemetrics::lsm_c_np(result, directions = 4)
    testthat::expect_equal(np[np$class == 3,]$value, 2)
  }
  # Create problem with 5 components
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_components_constraint(min_nb_components = 5, max_nb_components = 5)
  result <- solve(problem)
  if (require(landscapemetrics)) {
    np <- landscapemetrics::lsm_c_np(result, directions = 4)
    testthat::expect_equal(np[np$class == 3,]$value, 5)
  }
  # Create problem with between 6 and 10 components
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_components_constraint(min_nb_components = 6, max_nb_components = 10)
  result <- solve(problem)
  if (require(landscapemetrics)) {
    np <- landscapemetrics::lsm_c_np(result, directions = 4)
    testthat::expect_gte(np[np$class == 3,]$value, 6)
    testthat::expect_lte(np[np$class == 3,]$value, 10)
  }
})
