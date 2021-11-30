context("external datasets")

test_that("accessible.tif",{
  f <- system.file("extdata", "accessible.tif", package = "restoptr")
  x <- terra::rast(f)
  expect_is(x, "SpatRaster")
})

test_that("habitat.tif",{
  f <- system.file("extdata", "habitat.tif", package = "restoptr")
  x <- terra::rast(f)
  expect_is(x, "SpatRaster")
})

test_that("restorable.tif",{
  f <- system.file("extdata", "restorable.tif", package = "restoptr")
  x <- terra::rast(f)
  expect_is(x, "SpatRaster")
})

test_that("spatial properties", {
  f1 <- system.file("extdata", "accessible.tif", package = "restoptr")
  f2 <- system.file("extdata", "restorable.tif", package = "restoptr")
  f3 <- system.file("extdata", "restorable.tif", package = "restoptr")
  x1 <- terra::rast(f1)
  x2 <- terra::rast(f2)
  x3 <- terra::rast(f3)
  expect_true(terra::compareGeom(x1, x2, res = TRUE, stopiffalse = FALSE))
  expect_true(terra::compareGeom(x1, x3, res = TRUE, stopiffalse = FALSE))
  expect_false(terra::is.lonlat(x1))
  expect_false(terra::is.lonlat(x2))
  expect_false(terra::is.lonlat(x3))
})
