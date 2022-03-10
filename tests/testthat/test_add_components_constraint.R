context("restopt_problem")

test_that("add_components_constraint", {
  # Create problem with 1 component
  habitat <- terra::rast(system.file("extdata", "habitat.tif", package = "restoptr"))
  restorable <- terra::rast(system.file("extdata", "restorable.tif", package = "restoptr"))
  problem <- restopt_problem(habitat, restorable) %>%
    add_components_constraint(min_nb_components = 1, max_nb_components = 1)
  result <- solve(problem)
  np <- landscapemetrics::lsm_c_np(result, directions = 4)
  testthat::expect_equal(np[np$class == 2,]$value, 1)
  # Create problem with 2 components
  problem <- restopt_problem(habitat, restorable) %>%
    add_components_constraint(min_nb_components = 2, max_nb_components = 2)
  result <- solve(problem)
  np <- landscapemetrics::lsm_c_np(result, directions = 4)
  testthat::expect_equal(np[np$class == 2,]$value, 2)
  # Create problem with 5 components
  problem <- restopt_problem(habitat, restorable) %>%
    add_components_constraint(min_nb_components = 5, max_nb_components = 5)
  result <- solve(problem)
  np <- landscapemetrics::lsm_c_np(result, directions = 4)
  testthat::expect_equal(np[np$class == 2,]$value, 5)
  # Create problem with between 6 and 10 components
  problem <- restopt_problem(habitat, restorable) %>%
    add_components_constraint(min_nb_components = 6, max_nb_components = 10)
  result <- solve(problem)
  np <- landscapemetrics::lsm_c_np(result, directions = 4)
  testthat::expect_gte(np[np$class == 2,]$value, 6)
  testthat::expect_lte(np[np$class == 2,]$value, 10)
})
