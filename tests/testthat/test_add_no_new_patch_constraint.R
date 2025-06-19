context("no_new_patch constraint")

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
  # get initial nb patches
  np <- landscapemetrics::lsm_c_np(habitat_data, directions = 4)
  np <- np[np$class == 1, ]$value
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_locked_out_constraint(locked_out_data) %>%
    add_no_new_patch_constraint() %>%
    add_restorable_constraint(
      min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 0.7
    ) %>%
    add_settings(time_limit = 10, nb_solutions = 20)
  results <- solve(problem)
  for (r in results) {
    md <- get_metadata(r, area_unit = "ha")
    expect_lte(md$nb_patches, np)
  }
})

test_that("with lossless", {
  # import data
  habitat_data <- rast(system.file(
    "extdata", "case_study", "forest_2021.tif",
    package = "restoptr"
  ))
  available <- vect(system.file(
    "extdata", "case_study", "available.gpkg", package = "restoptr"
  ))
  # get initial nb patches
  np <- landscapemetrics::lsm_c_np(habitat_data, directions = 4)
  np <- np[np$class == 1, ]$value
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 1, 10, lossless_aggregation = TRUE) %>%
    add_available_areas_constraint(available) %>%
    add_restorable_constraint(
      min_restore = 90, max_restore = 210, unit = "ha", min_proportion = 1
    ) %>%
    add_no_new_patch_constraint() %>%
    add_settings(precision = 1, time_limit = 10, nb_solutions = 20)
  results <- solve(problem)
  for (r in results) {
    md <- get_metadata(r, area_unit = "ha")
    expect_lte(md$nb_patches, np)
  }
})
