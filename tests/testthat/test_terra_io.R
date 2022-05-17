context("terra_io")

test_that("terra_on_disk", {
  # import data
  x <- terra::rast(system.file(
    "extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # tests
  expect_true(terra_on_disk(x))
  expect_false(terra_on_disk(rast(ncol=2, nrow=2)))
})

test_that("terra_force_disk", {
  # import data
  x <- rast(ncol=2, nrow=2)
  values(x) <- 1:ncell(x)
  # create objects
  x <- x + 1
  path <- tempfile(fileext = ".tif")
  # tests
  expect_false(terra_on_disk(x))
  y <- terra_force_disk(x, path)
  expect_true(terra_on_disk(y))
  expect_equal(normalizePath(terra::sources(y)[[1]]), normalizePath(path))
  # clean up
  unlink(path, force = TRUE)
})

test_that("invalid inputs", {
  expect_error(terra_on_disk("a"))
})
