context("terra_io")

test_that("terra_io",{
  habitat <- terra::rast(system.file("extdata", "habitat.tif", package = "restoptr"))
  testthat::expect_true(terra_on_disk(habitat))
  minus_habitat <- -habitat
  testthat::expect_false(terra_on_disk(minus_habitat))
  x <- rast(ncol=2, nrow=2)
  values(x) <- 1:ncell(x)
  testthat::expect_false(terra_on_disk(x))
  testthat::expect_error(terra_on_disk(50))
  testthat::expect_error(terra_force_disk(55))
  path <- tempfile(fileext = ".tif")
  y <- terra_force_disk(x, filename = path)
  testthat::expect_true(terra_on_disk(y))
  testthat::expect_true(normalizePath(terra::sources(y)[[1]]) == normalizePath(path))
})
