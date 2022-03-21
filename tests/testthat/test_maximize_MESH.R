context("maximize_mesh")

test_that("maximize_mesh", {
  habitat <- terra::rast(system.file("extdata", "habitat_hi_res.tif", package = "restoptr"))
  accessible <- terra::vect(system.file("extdata", "accessible_areas.gpkg", package = "restoptr"))
  locked_out <- invert_vector(accessible, extent = ext(habitat), filter = accessible$ID==2)
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_locked_out_constraint(locked_out) %>%
    add_components_constraint(min_nb_components = 1, max_nb_components = 1) %>%
    add_compactness_constraint(max_diameter = 6) %>%
    add_restorable_constraint(min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 0.7) %>%
    set_max_mesh_objective()
  problem <- add_settings(problem, time_limit = 30)
  result <- solve(problem)
  testthat::expect_s4_class(result, "RestoptSolution")
  metadata <- get_metadata(result, area_unit = "ha")
  testthat::expect_lte(metadata$solving_time, 30)
  testthat::expect_gte(metadata$min_restore, 90)
  testthat::expect_lte(metadata$min_restore, 110)
  initial_value <- metadata$mesh_initial
  optimal_value <- metadata$mesh_best
  testthat::expect_true(initial_value <= optimal_value)
})
