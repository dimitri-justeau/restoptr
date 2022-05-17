context("add_components_constraint")

test_that("one component", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_components_constraint(
      min_nb_components = 1, max_nb_components = 1
    ) %>%
    add_settings(time_limit = 30)
  result <- solve(problem, verbose = TRUE)
  # tests
  expect_is(result, "SpatRaster")
  expect_gte(terra::global(result == 3, "sum", na.rm = TRUE), 1)
  skip_if_not_installed("landscapemetrics")
  n_components <- landscapemetrics::lsm_c_np(result, directions = 4)
  expect_equal(n_components$value[n_components$class == 3], 1)
})

test_that("two components", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_components_constraint(
      min_nb_components = 2, max_nb_components = 2
    ) %>%
    add_settings(time_limit = 30)
  result <- solve(problem, verbose = TRUE)
  # tests
  expect_is(result, "SpatRaster")
  expect_gte(terra::global(result == 3, "sum", na.rm = TRUE), 1)
  skip_if_not_installed("landscapemetrics")
  n_components <- landscapemetrics::lsm_c_np(result, directions = 4)
  expect_equal(n_components$value[n_components$class == 3], 2)
})

test_that("five components", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_components_constraint(
      min_nb_components = 5, max_nb_components = 5
    ) %>%
    add_settings(time_limit = 30)
  result <- solve(problem, verbose = TRUE)
  # tests
  expect_is(result, "SpatRaster")
  expect_gte(terra::global(result == 3, "sum", na.rm = TRUE), 1)
  skip_if_not_installed("landscapemetrics")
  n_components <- landscapemetrics::lsm_c_np(result, directions = 4)
  expect_equal(n_components$value[n_components$class == 3], 5)
})

test_that("six to ten components", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_components_constraint(
      min_nb_components = 6, max_nb_components = 10
    ) %>%
    add_settings(time_limit = 30)
  result <- solve(problem, verbose = TRUE)
  # tests
  expect_is(result, "SpatRaster")
  expect_gte(terra::global(result == 3, "sum", na.rm = TRUE), 1)
  skip_if_not_installed("landscapemetrics")
  n_components <- landscapemetrics::lsm_c_np(result, directions = 4)
  expect_gte(n_components$value[n_components$class == 3], 6)
  expect_lte(n_components$value[n_components$class == 3], 10)
})

test_that("invalid inputs", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # tests
  expect_error(
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_components_constraint(0, 5)
  )
  expect_error(
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_components_constraint(NA, 5)
  )
  expect_error(
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_components_constraint(5, 0)
  )
  expect_error(
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_components_constraint(5, NA)
  )
  expect_error(
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_components_constraint(5, 1)
  )
})
