context("add_settings")

test_that("time_limit", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_compactness_constraint(4.4, unit = "km") %>%
    set_max_mesh_objective() %>%
    add_settings(time_limit = 1)
  result <- solve(problem, verbose = TRUE)
  md <- get_metadata(result)
  # tests
  expect_lte(md$solving_time, 1.1)
})

test_that("precision", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  accessible_data <- terra::vect(system.file(
    "extdata", "accessible_areas.gpkg", package = "restoptr"
  ))
  locked_out_data <- invert_vector(accessible_data, extent = ext(habitat_data))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_locked_out_constraint(locked_out_data) %>%
    add_components_constraint(min_nb_components = 1, max_nb_components = 1) %>%
    add_compactness_constraint(max_diameter = 6, unit = "cells") %>%
    add_restorable_constraint(
      min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 0.7
    ) %>%
    set_max_mesh_objective() %>%
    add_settings(precision = 2)
  result <- solve(problem, verbose = TRUE)
  md <- get_metadata(result, area_unit = "cells")
  # tests
  expect_is(result, "SpatRaster")
  expect_gte(terra::global(result == 3, "sum", na.rm = TRUE), 1)
  expect_equal(round(md$mesh, 2), md$mesh)
})

test_that("nb_solutions", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  accessible_data <- terra::vect(system.file(
    "extdata", "accessible_areas.gpkg", package = "restoptr"
  ))
  locked_out_data <- invert_vector(accessible_data, extent = ext(habitat_data))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_compactness_constraint(4.4, unit = "km") %>%
    add_locked_out_constraint(locked_out_data) %>%
    add_components_constraint(min_nb_components = 1, max_nb_components = 1) %>%
    set_no_objective() %>%
    add_settings(time_limit = 5, nb_solutions = 2)
  result <- solve(problem, verbose = TRUE)
  # tests
  expect_is(result, "list")
  expect_is(result[[1]], "RestoptSolution")
  expect_is(result[[2]], "RestoptSolution")
  expect_length(result, 2)
  expect_gte(terra::global(result[[1]] == 3, "sum", na.rm = TRUE), 1)
  expect_gte(terra::global(result[[2]] == 3, "sum", na.rm = TRUE), 1)
})

test_that("optimality_gap", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  accessible_data <- terra::vect(system.file(
    "extdata", "accessible_areas.gpkg", package = "restoptr"
  ))
  locked_out_data <- invert_vector(accessible_data, extent = ext(habitat_data))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_compactness_constraint(4.4, unit = "km") %>%
    add_locked_out_constraint(locked_out_data) %>%
    add_components_constraint(min_nb_components = 1, max_nb_components = 1) %>%
    set_no_objective() %>%
    add_settings(time_limit = 5, optimality_gap = 0.5)
  result <- solve(problem, verbose = TRUE)
  # tests
  expect_is(result, "RestoptSolution")
  expect_gte(terra::global(result == 3, "sum", na.rm = TRUE), 1)
})

test_that("settings successfully overwritten", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build problems
  p1 <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_settings(
      time_limit = 5, nb_solutions = 3, optimality_gap = 0.5, precision = 3
    )
  p2 <-
    p1 %>%
    add_settings(
      time_limit = 4, nb_solutions = 5, optimality_gap = 0.1, precision = 2
    )
  # tests
  ## p1
  expect_equal(p1$settings$time_limit, 5)
  expect_equal(p1$settings$nb_solutions, 3)
  expect_equal(p1$settings$optimality_gap, 0.5)
  expect_equal(p1$settings$precision, 3)
  ## p2
  expect_equal(p2$settings$time_limit, 4)
  expect_equal(p2$settings$nb_solutions, 5)
  expect_equal(p2$settings$optimality_gap, 0.1)
  expect_equal(p2$settings$precision, 2)
})
