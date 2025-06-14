context("minimize_nb_patches")

test_that("expected_result", {
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
    add_restorable_constraint(
      min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 0.7
    ) %>%
    set_min_nb_patches_objective() %>%
    add_settings(time_limit = 10)
  result <- solve(problem)
  md <- get_metadata(result, area_unit = "ha")
  # tests
  expect_is(result, "RestoptSolution")
  expect_lte(md$solving_time, 10 * 1.1)
  expect_gte(md$min_restore, set_units(90, "ha"))
  expect_lte(md$min_restore, set_units(110, "ha"))
})

test_that("with lossless", {
  # import data
  habitat_data <- rast(system.file(
    "extdata", "case_study", "forest_2021.tif",
    package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 1, 10, lossless_aggregation = TRUE) %>%
    add_restorable_constraint(
      min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 1
    ) %>%
    add_compactness_constraint(1500, unit = "m") %>%
    add_components_constraint(1, 3) %>%
    add_settings(precision = 1, time_limit = 30) %>%
    set_min_nb_patches_objective()
  result <- solve(problem)
  md <- get_metadata(result)
  # tests
  expect_is(result, "RestoptSolution")
  expect_equal(get_aggregation_method(problem), "lossless")
  expect_true(md$optimality_proven)
})
