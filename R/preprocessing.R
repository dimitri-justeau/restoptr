#' @include internal.R
NULL


#' Restopr input preprocessing function.
#'
#' From a binary, possibly high resolution, habitat raster, this function
#' produces two input rasters for restopot:
#'
#' - The binary habitat raster, which can be the same as the input
#'   (aggregation factor = 1), but most often is a downsampled version of it,
#'   to ensure the tractability of the problem.
#'
#' - The restorable habitat raster, which is a raster indicating how much
#'   habitat can be restored. If the aggregation factor = 1, the the restorable
#'   habitat raster is binary and the inverse of the habitat raster. Else, the
#'   surface of habitat is computed according to the spatial resolution, and the
#'   number of habitat pixel present in one larger aggregated cell.
#'
#' @param habitat [terra::rast()] Raster object containing binary
#' values that indicate if each planning unit contains habitat or not. Cells
#' with the value `1` must correspond to existing habitat. Cells with the value
#' `0` must correspond to degraded (or simply non-habitat) areas. Finally,
#' `NA` (or `NO_DATA`) cells are considered to be outside of the landscape.
#'
#' @param habitat_threshold Number between 0 and 1 indicating, when the habitat
#' raster is downsampled, the minimum proportion of habitat cells (from the
#' original raster) are required within the downsampled raster to be considered
#' as habitat.
#'
#' @param aggregation_factor positive integer corresponding to the level of
#' downsampling that will be applied to the habitat. This parameter is
#' important to ensure the tractability of a problem.
#'
#' @return A vector : c(downsampled_habitat, restorable_area)
#'
#' @details TODO
#'
#' @examples
#' \dontrun{
#' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat.tif", package = "restoptr")
#' )
#' TODO
#' }
#'
#' @export
prepare_inputs <- function(habitat, habitat_threshold = 1, aggregation_factor = 1) {
  ## initial checks
  assertthat::assert_that(
    inherits(habitat, "SpatRaster")
  )
  ## further checks
  assertthat::assert_that(
    terra::hasValues(habitat),
    terra::nlyr(habitat) == 1
  )
  ## assert valid values
  assertthat::assert_that(
    is_binary_raster(habitat),
    msg = "argument to \"habitat\" must have binary values"
  )
  all_ones <- habitat >= 0
  cell_area <- terra::aggregate(
    all_ones,
    fact = aggregation_factor,
    fun = "sum",
    na.rm = TRUE
  )
  down_sum <- terra::aggregate(
    habitat,
    fact = aggregation_factor,
    fun = "sum",
    na.rm = TRUE
  )
  downsampled_habitat <- (down_sum / cell_area) >= habitat_threshold
  restorable_habitat <- cell_area - down_sum
  return(c(downsampled_habitat, restorable_habitat))
}
