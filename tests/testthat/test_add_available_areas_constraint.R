context("add_available_areas_constraint")

test_that("add_available_areas_constraint", {
  # Test from vector data
  habitat <- terra::rast(system.file("extdata", "habitat_hi_res.tif", package = "restoptr"))
  accessible <- terra::vect(system.file("extdata", "accessible_areas.gpkg", package = "restoptr"))
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_available_areas_constraint(accessible) %>%
    add_compactness_constraint(4.4, unit = "km") %>%
    set_max_mesh_objective() %>%
    add_settings(time_limit = 30)
  result <- solve(problem, verbose=TRUE)
  testthat::expect_true(inherits(result, "SpatRaster"))
  wrong <- result == 3 & get_locked_out_areas(problem)
  testthat::expect_equal(length(which(wrong[,] == 1)), 0)
  # Test from raster data
  habitat <- terra::rast(system.file("extdata", "habitat_hi_res.tif", package = "restoptr"))
  locked_out <- terra::rast(system.file("extdata", "locked_out.tif", package = "restoptr"))
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_available_areas_constraint(round(!locked_out)) %>%
    add_compactness_constraint(4.4, unit = "km") %>%
    set_max_mesh_objective() %>%
    add_settings(time_limit = 30)
  result <- solve(problem, verbose=TRUE)
  testthat::expect_true(inherits(result, "SpatRaster"))
  wrong <- result == 3 & get_locked_out_areas(problem)
  testthat::expect_equal(length(which(wrong[,] == 1)), 0)
  # Test wrong iput raster data
  habitat <- terra::rast(system.file("extdata", "habitat_hi_res.tif", package = "restoptr"))
  locked_out <- terra::rast(system.file("extdata", "locked_out.tif", package = "restoptr"))
  locked_out <- terra::aggregate(locked_out, factor = 2)
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7)
  testthat::expect_error(p %>% add_available_areas_constraint(round(!locked_out)))
})
