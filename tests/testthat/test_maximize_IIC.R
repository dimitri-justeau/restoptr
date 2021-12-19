context("maximize_iic")

test_that("maximize_iic", {
  gc()

  habitat <- terra::rast(system.file("extdata", "habitat.tif", package = "restoptr"))
  restorable <- terra::rast(system.file("extdata", "restorable.tif", package = "restoptr"))
  accessible <- terra::rast(system.file("extdata", "accessible.tif", package = "restoptr"))

  problem <- restopt_problem(habitat, restorable) %>%
    add_locked_out_constraint(accessible) %>%
    add_components_constraint(min_nb_components = 1, max_nb_components = 1) %>%
    add_compactness_constraint(max_diameter = 6) %>%
    add_restorable_constraint(min_restore = 90, max_restore = 110, cell_area = 23, min_proportion = 0.7) %>%
    add_max_iic_objective()

  problem <- add_settings(problem, time_limit = 30)

  result <- solve(problem)

  testthat::expect_s4_class(result, "SpatRaster")

  metadata <- attributes(result)$metadata

  testthat::expect_lte(metadata$solving.time..ms., 30000)
  testthat::expect_gte(metadata$Minimum.area.to.restore, 90)
  testthat::expect_lte(metadata$Minimum.area.to.restore, 110)
  initial_value <- metadata$initial.IIC.value
  optimal_value <- metadata$optimal.IIC.value
  testthat::expect_true(initial_value <= optimal_value)
})
