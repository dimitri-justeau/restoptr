#' @include internal.R
NULL

#' Invert a vector layer according to the extent of a restopt problem.
#'
#' @param vector_layer [terra::vect()] Vector layer.
#'
#' @param extent [`SpatExtent`] Optional: you can specify another extent as the
#' input vector layer extent for the inversion.
#'
#' @param filter Optional: filter to apply to `x`. Leave NULL for no filtering.
#'
#' @details
#' Invert a vector layer according to its extent, or a user-specified extent.
#' This function is useful to derive locked out areas from accessible areas,
#' e.g. buffer around tracks.
#'
#' @return A [terra::vect()] Vector object.
#'
#' @examples
#' \dontrun{
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#' available <- vect(
#'   system.file("extdata", "accessible_areas.gpkg", package = "restoptr")
#' )
#' locked_out <- invert_vector(
#'   vector_layer = available,
#'   extent = ext(habitat_data),
#'   filter = available$ID==2
#' )
#' }
#'
#' @export
invert_vector <- function(vector_layer, extent=NULL, filter=NULL) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(vector_layer, "SpatVector"),
    inherits(extent, "SpatExtent") || is.null(extent)
  )
  if (is.null(extent)) {
    extent <- ext(vector_layer)
  }
  if (all(is.valid(vector_layer))) {
    y <- vector_layer
  } else {
    y <- makeValid(vector_layer)
  }
  if (!is.null(filter)) {
    y <- y[filter]
  }
  return(as.polygons(extent)  - y)
}

#' Compute the area of a cell in ha, m^2, or km^2
#'
#' @param raster_layer [terra::rast()] Raster object.
#'
#' @param unit Unit of the area ("ha" for hectares, "m" for square meters,
#' "km" for square kilometers)
#'
#' @details
#' The input raster must have a projected coordinate system. The distortion is
#' not corrected. It could be using the `cellSize` function of the `terra`
#' package, but this function is currently pretty slow for large rasters. If
#' your problem is at regional scale, the distortion should be negligible.
#' However, at larger scales, the best is to use an equal-area projected
#' coordinate system.
#'
#' @return The area of a cell in the desired unit.
#'
#' @examples
#' \dontrun{
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#' cell_area(habitat_data, "ha")
#' }
#'
#' @export
cell_area <- function(raster_layer, unit = "ha") {
  # Check arguments
  assertthat::assert_that(
    inherits(raster_layer, "SpatRaster"),
    assertthat::is.string(unit),
    unit %in% c("ha", "m", "km")
  )
  # Throw an error if the raster is not projected
  if (is.lonlat(raster_layer)) {
    stop("The input raster does not use a projected coordinate system. Please reproject.")
  }
  if (unit == "ha") {
    dividend <- 10000
  }
  if (unit == "m") {
    dividend <- 1
  }
  if (unit == "km") {
    dividend <- 1000000
  }
  cell_area <- (prod(res(raster_layer)) * linearUnits(raster_layer)) / dividend
  return(cell_area)
}

#' Compute the number of cells corresponding to a given area.
#'
#' @param raster_layer [terra::rast()] Raster object.
#'
#' @param area Area, in ha, m^2, or km^2.
#'
#' @param unit Unit of the area ("ha" for hectares, "m" for square meters,
#' "km" for square kilometers)
#'
#' @details
#' The input raster must have a projected coordinate system. The distortion is
#' not corrected. It could be using the `cellSize` function of the `terra`
#' package, but this function is currently pretty slow for large rasters. If
#' your problem is at regional scale, the distortion should be negligible.
#' However, at larger scales, the best is to use an equal-area projected
#' coordinate system.
#'
#' @return The number of raster cell correspond to the given area.
#'
#' @examples
#' \dontrun{
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#' area_to_nb_cells(habitat_data, 20, unit = "ha")
#' }
#'
#' @export
area_to_nb_cells <- function(raster_layer, area, unit = "ha") {
  assertthat::assert_that(
    assertthat::is.number(area)
  )
  return(area / cell_area(raster_layer, unit))
}

#' Compute the area corresponding to a given number of cells.
#'
#' @param raster_layer [terra::rast()] Raster object.
#'
#' @param nb_cells Number of raster cells.
#'
#' @param unit Unit of the area ("ha" for hectares, "m" for square meters,
#' "km" for square kilometers)
#'
#' @details
#' The input raster must have a projected coordinate system. The distortion is
#' not corrected. It could be using the `cellSize` function of the `terra`
#' package, but this function is currently pretty slow for large rasters. If
#' your problem is at regional scale, the distortion should be negligible.
#' However, at larger scales, the best is to use an equal-area projected
#' coordinate system.
#'
#' @return The area corresponding to `nb_cells` in the desired unit.
#'
#' @examples
#' \dontrun{
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#' nb_cell_to_area(habitat_data, 20, unit = "ha")
#' }
#'
#' @export
nb_cell_to_area <- function(raster_layer, nb_cells, unit = "ha") {
  assertthat::assert_that(
    assertthat::is.number(nb_cells)
  )
  return(nb_cells * cell_area(raster_layer, unit))
}
