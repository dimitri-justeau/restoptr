context("add_locked_out_constraint")

test_that("add_locked_out_constraint", {
  # Test from vector data
  habitat <- terra::rast(system.file("extdata", "habitat_hi_res.tif", package = "restoptr"))
  accessible <- terra::vect(system.file("extdata", "accessible_areas.gpkg", package = "restoptr"))
  locked_out <- invert_vector(accessible, extent = ext(habitat), filter = accessible$ID==1)
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_locked_out_constraint(locked_out) %>%
    add_compactness_constraint(10) %>%
    set_max_mesh_objective() %>%
    add_settings(time_limit = 30)
  result <- solve(problem, verbose=TRUE)
  testthat::expect_true(inherits(result, "SpatRaster"))
  # Test with invalid raster
})
