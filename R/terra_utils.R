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

