#' @include internal.R
NULL

#' On disk?
#'
#' Check if a raster ([terra::rast()]) object is stored on disk?
#'
#' @param x [terra::rast()] Raster object.
#'
#' @details
#' The data is only considered available on disk if the data
#' is not stored in memory and is only obtained from a single file source.
#'
#' @return A `logical` indicating if the data is stored on disk.
#'
#' @noRd
terra_on_disk <- function(x) {
  assertthat::assert_that(inherits(x, "SpatRaster"))
  s <- terra::sources(x)[[1]]
  out <- all(nchar(s > 0) && all(file.exists(s)))
  if (!out) return(out)
  out && (terra::nlyr(x) == terra::nlyr(terra::rast(s)))
}

#' Force file-backed raster
#'
#' Force a raster ([terra::rast()]) object to have data stored on disk.
#'
#' @param x [terra::rast()] Raster object.
#'
#' @param filename `character` File path to store data.
#'  Defaults to a temporarily (GeoTIFF) file.
#'
#' @param ... Arguments passed to [terra::writeRaster()] for saving data.
#'
#' @return A `[terra::rast()] raster object.
#'
#' @noRd
terra_force_disk <- function(x, filename = tempfile(fileext = ".tif"), ...) {
  assertthat::assert_that(
    inherits(x, "SpatRaster"),
    assertthat::is.string(filename),
    assertthat::noNA(filename)
  )
  if (!terra_on_disk(x)) {
    terra::writeRaster(x = x, filename = filename, ...)
    x <- terra::rast(filename)
  }
  x
}
