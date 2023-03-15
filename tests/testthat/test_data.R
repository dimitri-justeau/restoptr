context("external datasets")

test_that("habitat_hi_res.tif",{
  f <- system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
  x <- terra::rast(f)
  expect_is(x, "SpatRaster")
})

test_that("locked_out.tif",{
  f <- system.file("extdata", "locked_out.tif", package = "restoptr")
  x <- terra::rast(f)
  expect_is(x, "SpatRaster")
})

test_that("accessible_areas.gpkg",{
  f <- system.file("extdata", "accessible_areas.gpkg", package = "restoptr")
  x <- terra::vect(f)
  expect_is(x, "SpatVector")
})

test_that("spatial properties", {
  f1 <- system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
  f2 <- system.file("extdata", "locked_out.tif", package = "restoptr")
  f3 <- system.file("extdata", "accessible_areas.gpkg", package = "restoptr")
  x1 <- terra::rast(f1)
  x2 <- terra::rast(f2)
  x3 <- terra::vect(f3)
  expect_true(terra::compareGeom(x1, x2, res = TRUE, stopOnError = FALSE))
  expect_false(terra::is.lonlat(x1))
  expect_false(terra::is.lonlat(x2))
  expect_false(terra::is.lonlat(x3))
})
