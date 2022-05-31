#' Is a binary raster?
#'
#' Check if [terra::rast()] raster object contains binary values.
#'
#' @param x [terra::rast()] Raster object.
#'
#' @return `logical` value indicating if raster contains binary values.
#'
#' @noRd
is_binary_raster <- function(x) {
  assertthat::assert_that(inherits(x, "SpatRaster"))
  terra::global(x, function(x) {
    x <- x[is.finite(x)]
    all(x == 0 | x == 1)
  })[[1]]
}
