context("add_restorable_constraint")

test_that("exact amount of restoration area", {
  # import data
  habitat_data <- terra::rast(system.file(
    "extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_restorable_constraint(
      min_restore = 90, max_restore = 90, unit = "ha"
    )
  result <- solve(problem)
  # tests
  expect_is(result, "SpatRaster")
  expect_gte(terra::global(result == 3, "sum", na.rm = TRUE), 1)
  rest_cells <- which(result[,] == 3)
  val <- nb_cell_to_area(
    habitat_data,
    sum(round(get_restorable_habitat(problem))[rest_cells]),
    unit = "ha"
  )
  expect_equal(val, set_units(90, "ha"), tolerance = 1)
})

test_that("interval amount of restoration area", {
  # import data
  habitat_data <- terra::rast(system.file(
    "extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_restorable_constraint(
      min_restore = 200, max_restore = 211, unit = "ha"
    )
  result <- solve(problem)
  # tests
  expect_is(result, "SpatRaster")
  expect_gte(terra::global(result == 3, "sum", na.rm = TRUE), 1)
  rest_cells <- which(result[,] == 3)
  val <- nb_cell_to_area(
    habitat_data,
    sum(round(get_restorable_habitat(problem))[rest_cells]),
    unit = "ha"
  )
  expect_gte(round(val), set_units(200, "ha"))
  expect_lte(round(val), set_units(211, "ha"))
})

test_that("minimum proportion with exact amount of restoration area", {
  # import data
  habitat_data <- terra::rast(system.file(
    "extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_restorable_constraint(
      min_restore = 90, max_restore = 90, unit = "ha",
      min_proportion = 0.7
    )
  result <- solve(problem)
  # tests
  expect_is(result, "SpatRaster")
  expect_gte(terra::global(result == 3, "sum", na.rm = TRUE), 1)
  rest_cells <- which(result[,] == 3)
  cell_area <- problem$data$cell_area
  val <- nb_cell_to_area(
    habitat_data,
    sum(max(
      round(get_restorable_habitat(problem)) - ceiling(0.3 * cell_area),
      0)[rest_cells]),
    unit = "ha"
  )
  expect_equal(val, set_units(90, "ha"), tolerance = 1)
})


test_that("minimum proportion with interval amount of restoration area", {
  # import data
  habitat_data <- terra::rast(system.file(
    "extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_restorable_constraint(
      min_restore = 50, max_restore = 60, unit = "ha",
      min_proportion = 0.5
    )
  result <- solve(problem)
  # tests
  expect_is(result, "SpatRaster")
  expect_gte(terra::global(result == 3, "sum", na.rm = TRUE), 1)
  rest_cells <- which(result[,] == 3)
  cell_area <- problem$data$cell_area
  val <- nb_cell_to_area(
    habitat_data,
    sum(max(
      round(get_restorable_habitat(problem)) - ceiling(0.5 * cell_area),
      0)[rest_cells]),
    unit = "ha"
  )
  expect_gte(round(val), set_units(50, "ha"))
  expect_lte(round(val), set_units(60, "ha"))
})

test_that(
  "cell unit minimum proportion with interval amount of restoration area", {
  # import data
  habitat_data <- terra::rast(system.file(
    "extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 1, 16) %>%
    add_restorable_constraint(
      min_restore = 50, max_restore = 60, unit = "cells",
      min_proportion = 1
    )
  result <- solve(problem)
  # tests
  expect_is(result, "SpatRaster")
  expect_gte(terra::global(result == 3, "sum", na.rm = TRUE), 1)
  rest_cells <- which(result[,] == 3)
  val <- sum(round(get_restorable_habitat(problem))[rest_cells])
  testthat::expect_gte(val, 50)
  testthat::expect_lte(val, 60)
})

test_that("invalid inputs", {
  # import data
  habitat_data <- terra::rast(system.file(
    "extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # tests
  expect_error(
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_restorable_constraint(
      min_restore = -1, max_restore = 60, unit = "ha",
      min_proportion = 1
    )
  )
  expect_error(
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_restorable_constraint(
      min_restore = 50, max_restore = -1, unit = "ha",
      min_proportion = 1
    )
  )
  expect_error(
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_restorable_constraint(
      min_restore = 50, max_restore = 60, unit = "kg",
      min_proportion = 1
    )
  )
  expect_error(
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_restorable_constraint(
      min_restore = 50, max_restore = 60, unit = "kg",
      min_proportion = -0.5
    )
  )
  expect_error(
    restopt_problem(round(terra::project(habitat_data, "epsg:4326")), 0.7, 16) %>%
    add_restorable_constraint(
      min_restore = 50, max_restore = 60, unit = "ha",
      min_proportion = 0.8
    )
  )
})
