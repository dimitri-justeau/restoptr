context("maximize_mesh")

test_that("maximize_mesh", {
  habitat <- terra::rast(system.file("extdata", "habitat.tif", package = "restoptr"))
  restorable <- terra::rast(system.file("extdata", "restorable.tif", package = "restoptr"))
  accessible <- terra::rast(system.file("extdata", "accessible.tif", package = "restoptr"))
  problem <- RestoptProblem(habitat=habitat, restorable=restorable, accessible=accessible)
  problem <- postNbComponentsConstraint(problem, 1, 1)
  problem <- postRestorableConstraint(problem, 90, 110, 23, 0.7)
  problem <- postCompactnessConstraint(problem, 6)
  result <- maximizeMESH(problem, 3)
  testthat::expect_is(result[[1]], "SpatRaster")
})
