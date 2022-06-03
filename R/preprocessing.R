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
#' @param habitat_threshold `numeric` Number between 0 and 1 indicating, when the habitat
#' raster is downsampled, the minimum proportion of habitat cells (from the
#' original raster) are required within the downsampled raster to be considered
#' as habitat.
#'
#' @param aggregation_factor `integer` positive integer corresponding to the level of
#' downsampling that will be applied to the habitat. This parameter is
#' important to ensure the tractability of a problem.
#'
#' @return A vector : c(downsampled_habitat, restorable_area, cell_area)
#'
#' @details This preprocessing function produces the necessary inputs of a
#' restopt problem from a single binary habitat raster (`habitat`), which can
#' be at high resolution. Restopt solves a hard constrained combinatorial problem
#' (it can be reduced to a constrained 0/1 knapsack problem, which is know to
#' be NP-Complete), thus the input resolution might need to be reduced to
#' ensure a tractable problem. Performing this downsampling in a systematic
#' and reproducible way is the aim of this function, which relies on the
#' `terra::aggregate()` function to do it. The `aggregation_factor` parameter
#' indicates how much the resolution must be reduced. An aggregated pixel
#' will contain at most `aggregation_factor^2` pixels from the input `habitat`
#' raster (`cell_area` raster in this function outputs). If an aggregated pixel
#' is close to the spatial boundaries of the problem (i.e. NA cells), it can
#' contain less than `aggregation_factor^2` fine grained pixels. The
#' `habitat_threshold` parameter indicates the minimum proportion of `habitat`
#' pixels (relative to `cell_area`) whose value is 1 to consider an aggregated
#' pixel as habitat (`downsampled_habitat` output raster). The `restorable_area`
#' output raster correspond to the number of pixel with value 0 in aggregated pixels.
#'
#' @examples
#' \donttest{
#' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' data <- prepare_inputs(
#'     habitat = habitat_data,
#'     habitat_threshold = 0.7,
#'     aggregation_factor = 16
#' )
#' }
#'
#' @noRd
prepare_inputs <- function(habitat, habitat_threshold = 1, aggregation_factor = 1) {
  ## initial checks
  assertthat::assert_that(
    inherits(habitat, "SpatRaster"),
    habitat_threshold >= 0 && habitat_threshold <= 1,
    aggregation_factor >= 1
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
  if (aggregation_factor > 1) {
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
  } else {
    if (habitat_threshold < 1) {
      warning(paste("The habitat threshold parameter has no effect when the",
                    "aggregation factor is 1"))
    }
    downsampled_habitat <- habitat
    restorable_habitat <- habitat == 0 * 1
    cell_area <- habitat >= 0
  }
  return(c(downsampled_habitat, restorable_habitat, cell_area))
}
