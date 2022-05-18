context("min_iic_constraint")

test_that("expected_result_distance_threshold_in_cells", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
    ))
  accessible_data <- terra::vect(system.file(
    "extdata", "accessible_areas.gpkg", package = "restoptr"
  ))
  # prepare locked out data
  locked_out_data <- invert_vector(
    accessible_data,
    extent = terra::ext(habitat_data),
    filter = accessible_data$ID == 1
  )
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_locked_out_constraint(locked_out_data) %>%
    add_components_constraint(min_nb_components = 1, max_nb_components = 1) %>%
    add_compactness_constraint(max_diameter = 2700, unit = "m") %>%
    add_restorable_constraint(
      min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 0.7
    ) %>%
    add_min_iic_constraint(min_iic = 0.2, distance_threshold = 3, unit = "cells") %>%
    add_settings(time_limit = 10, nb_solutions = 10)
  result <- solve(problem, verbose = TRUE)
  expect_length(result, 10)
  for (i in seq(1, 10)) {
    r <- result[[i]]
    expect_true(inherits(r, "RestoptSolution"))
    md <- get_metadata(r)
    expect_lte(md$solving_time, 10 * 1.1)
    expect_gte(md$min_restore, set_units(90, "ha"))
    expect_lte(md$min_restore, set_units(110, "ha"))
    expect_true(md$iic >= 0.2)
  }
})

test_that("expected_result_distance_threshold_default", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
    ))
  accessible_data <- terra::vect(system.file(
    "extdata", "accessible_areas.gpkg", package = "restoptr"
  ))
  # prepare locked out data
  locked_out_data <- invert_vector(
    accessible_data,
    extent = terra::ext(habitat_data),
    filter = accessible_data$ID == 2
  )
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_locked_out_constraint(locked_out_data) %>%
    add_components_constraint(min_nb_components = 1, max_nb_components = 1) %>%
    add_compactness_constraint(max_diameter = 2700, unit = "m") %>%
    add_restorable_constraint(
      min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 0.7
    ) %>%
    add_min_iic_constraint(min_iic = 0.2) %>%
    add_settings(time_limit = 10, nb_solutions = 10)
  result <- solve(problem, verbose = TRUE)
  expect_length(result, 10)
  for (i in seq(1, 10)) {
    r <- result[[i]]
    expect_true(inherits(r, "RestoptSolution"))
    md <- get_metadata(r)
    expect_lte(md$solving_time, 10 * 1.1)
    expect_gte(md$min_restore, set_units(90, "ha"))
    expect_lte(md$min_restore, set_units(110, "ha"))
    expect_true(md$iic >= 0.2)
  }
})

test_that("expected_result_distance_threshold_meters", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
    ))
  accessible_data <- terra::vect(system.file(
    "extdata", "accessible_areas.gpkg", package = "restoptr"
  ))
  # prepare locked out data
  locked_out_data <- invert_vector(
    accessible_data,
    extent = terra::ext(habitat_data),
    filter = accessible_data$ID == 2
  )
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_locked_out_constraint(locked_out_data) %>%
    add_components_constraint(min_nb_components = 1, max_nb_components = 1) %>%
    add_compactness_constraint(max_diameter = 2700, unit = "m") %>%
    add_restorable_constraint(
      min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 0.7
    ) %>%
    add_min_iic_constraint(min_iic = 0.2, distance_threshold = 500, unit = "m") %>%
    add_settings(time_limit = 10, nb_solutions = 10)
  result <- solve(problem, verbose = TRUE)
  expect_length(result, 10)
  for (i in seq(1, 10)) {
    r <- result[[i]]
    expect_true(inherits(r, "RestoptSolution"))
    md <- get_metadata(r)
    expect_lte(md$solving_time, 10 * 1.1)
    expect_gte(md$min_restore, set_units(90, "ha"))
    expect_lte(md$min_restore, set_units(110, "ha"))
    expect_true(md$iic >= 0.2)
  }
})

test_that("expected_result_distance_threshold_meters_less_than_one_cell", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
    ))
  accessible_data <- terra::vect(system.file(
    "extdata", "accessible_areas.gpkg", package = "restoptr"
  ))
  # prepare locked out data
  locked_out_data <- invert_vector(
    accessible_data,
    extent = terra::ext(habitat_data),
    filter = accessible_data$ID == 2
  )
  # build and solve problem
  expect_warning(
    problem <- restopt_problem(habitat_data, 0.7, 16) %>%
      add_locked_out_constraint(locked_out_data) %>%
      add_components_constraint(min_nb_components = 1, max_nb_components = 1) %>%
      add_compactness_constraint(max_diameter = 2700, unit = "m") %>%
      add_restorable_constraint(
        min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 0.7
      ) %>%
      add_min_iic_constraint(min_iic = 0.2, distance_threshold = 200, unit = "m") %>%
      add_settings(time_limit = 10, nb_solutions = 10)
  )
  result <- solve(problem, verbose = TRUE)
  expect_length(result, 10)
  for (i in seq(1, 10)) {
    r <- result[[i]]
    expect_true(inherits(r, "RestoptSolution"))
    md <- get_metadata(r)
    expect_lte(md$solving_time, 10 * 1.1)
    expect_gte(md$min_restore, set_units(90, "ha"))
    expect_lte(md$min_restore, set_units(110, "ha"))
    expect_true(md$iic >= 0.2)
  }
})

test_that("expect_no_solution", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
    ))
  accessible_data <- terra::vect(system.file(
    "extdata", "accessible_areas.gpkg", package = "restoptr"
  ))
  # prepare locked out data
  locked_out_data <- invert_vector(
    accessible_data,
    extent = terra::ext(habitat_data),
    filter = accessible_data$ID == 1
  )
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_locked_out_constraint(locked_out_data) %>%
    add_components_constraint(min_nb_components = 1, max_nb_components = 1) %>%
    add_compactness_constraint(max_diameter = 2700, unit = "m") %>%
    add_restorable_constraint(
      min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 0.7
    ) %>%
    add_min_iic_constraint(min_iic = 0.9, distance_threshold = 3, unit = "cells") %>%
    add_settings(time_limit = 10, nb_solutions = 10)
  expect_error(solve(problem))
})
