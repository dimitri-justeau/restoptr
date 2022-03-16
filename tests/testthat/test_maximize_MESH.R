context("maximize_mesh")

test_that("maximize_mesh", {
  gc()

  habitat <- terra::rast(system.file("extdata", "habitat.tif", package = "restoptr"))
  restorable <- terra::rast(system.file("extdata", "restorable.tif", package = "restoptr"))
  accessible <- terra::rast(system.file("extdata", "accessible.tif", package = "restoptr"))

  problem <- restopt_problem(habitat, restorable) %>%
    add_locked_out_constraint(round(accessible == 2)) %>%
    add_components_constraint(min_nb_components = 1, max_nb_components = 1) %>%
    add_compactness_constraint(max_diameter = 6) %>%
    add_restorable_constraint(min_restore = 90, max_restore = 110, cell_area = 23, min_proportion = 0.7) %>%
    set_max_mesh_objective()

  problem <- add_settings(problem, time_limit = 30)

  result <- solve(problem)

  testthat::expect_s4_class(result, "SpatRaster")

  metadata <- attributes(result)$metadata

  testthat::expect_lte(metadata$solving.time..ms., 30000)
  testthat::expect_gte(metadata$Minimum.area.to.restore, 90)
  testthat::expect_lte(metadata$Minimum.area.to.restore, 110)
  initial_value <- metadata$MESH_initial
  optimal_value <- metadata$MESH_best
  testthat::expect_true(initial_value <= optimal_value)
})
