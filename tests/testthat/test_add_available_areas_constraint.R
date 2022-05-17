context("add_available_areas_constraint")

test_that("vector data", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  accessible_data <- terra::vect(system.file(
    "extdata", "accessible_areas.gpkg", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_available_areas_constraint(accessible_data) %>%
    add_compactness_constraint(4.4, unit = "km") %>%
    set_max_mesh_objective() %>%
    add_settings(time_limit = 30)
  result <- solve(problem, verbose = TRUE)
  # tests
  expect_is(result, "SpatRaster")
  expect_gte(terra::global(result == 3, "sum", na.rm = TRUE)[[1]], 1)
  expect_equal(
    terra::global(
      result == 3 & get_locked_out_areas(problem),
      "sum", na.rm = TRUE
    )[[1]],
    0
  )
})

test_that("raster data", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  locked_out_data <- terra::rast(system.file(
    "extdata", "locked_out.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_available_areas_constraint(round(!locked_out_data)) %>%
    add_compactness_constraint(4.4, unit = "km") %>%
    set_max_mesh_objective() %>%
    add_settings(time_limit = 30)
  result <- solve(problem, verbose = TRUE)
  # tests
  expect_is(result, "SpatRaster")
  expect_gte(terra::global(result == 3, "sum", na.rm = TRUE)[[1]], 1)
  expect_equal(
    terra::global(
      result == 3 & get_locked_out_areas(problem),
      "sum", na.rm = TRUE
    )[[1]],
    0
  )
})

test_that("invalid input", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  locked_out_data <- terra::rast(system.file(
    "extdata", "locked_out.tif", package = "restoptr"
  ))
  # modify locked data so it has different spatial properties to habitat data
  locked_out_data <- terra::aggregate(locked_out_data, factor = 2)
  # tests
  expect_error(
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_available_areas_constraint(round(!locked_out_data)),
    "spatial properties"
  )
})
