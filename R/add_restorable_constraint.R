#' @include internal.R
NULL

#' Add constraint to specify the available amount of surface for restoration
#'
#' Add constraint to a restoration problem ([restopt_problem()]) object
#' to specify specify the available amount of surface for restoration
#'
#' @inheritParams set_max_mesh_objective
#'
#' @param min_restore `integer` Minimum allowed area to restore in the solution.
#'
#' @param max_restore `integer` Maximum allowed area to restore in the solution
#'
#' @param unit Area unit ("ha" for hectares, "m" for square meters,
#' "km" for square kilometers, "cells" for number of cells from the original
#' habitat raster). If the input habitat raster does not use a projected
#' coordinate system, only "cells" is available.
#'
#' @param min_proportion `float` Minimum habitat proportion to consider a cell
#' as restored.
#'
#' @details Given the `restorable_habitat` input raster in \link{restopt_problem},
#' this constraint ensures that the total amount of restorable habitat in
#' selected planning units is at least `min_restore` and at most `max_restore`.
#' The unit of `min_restore` and `max_restore` can be either hectares ("ha"),
#' square meters ("m"), square kilometers ("km"), or number of cells from the
#' original habitat input raster ("cells"). The `min_proportion` parameter is a
#' numeric between 0 and 1, and correspond to the minimum proportion of habitat
#' area that needs to be restored in the planning unit to consider the planning
#' unit as restored. This proportion is relative to the area of a planning unit,
#' which is computed automatically from the input habitat raster. Note that
#' planning unit area is considered uniform, and the distortion is not corrected.
#' It could be using the `cellSize` function of the `terra` package, but this
#' function is currently pretty slow for large rasters. If your problem is at
#' regional scale, the distortion should be negligible. However, at larger
#' scales, the best is to use an equal-area projected coordinate system.
#'
#' Note that when a solution is found, the "maximum restorable habitat" is
#' displayed, this value does not correspond to the `max_restore` parameter,
#' but to the total area that can be restored in the selected planning units.
#' The `max_restore` parameter is actually an upper bound of the minimum habitat
#' that needs to be restored to reach the `min_proportion` of habitat in every
#' selected planning units.
#'
#' @examples
#' \dontrun{
#' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#'
#' # create problem
#' p <- restopt_problem(
#'     existing_habitat = habitat_data,
#'     aggregation_factor = 16,
#'     habitat_threshold = 0.7
#'   ) %>%
#'   add_restorable_constraint(
#'     min_restore = 200,
#'     max_restore = 300,
#'     min_proportion = 0.7
#'   ) %>%
#'   add_compactness_constraint(5)
#'
#' # print problem
#' print(p)
#'
#' # plot preprocessed data
#' plot(rast(list(p$data$existing_habitat, p$data$restorable_habitat, p$data$locked_out)), nc = 3)
#'
#' # Solve problem
#' s <- solve(p)
#' # plot solution
#' plot(
#'   x = s, main = "solution",
#'   col = c("#E5E5E5", "#fff1d6", "#b2df8a", "#1f78b4")
#' )
#' }
#'
#' @export
add_restorable_constraint <- function(problem,
                                      min_restore,
                                      max_restore,
                                      min_proportion = 1,
                                      unit="ha") {
  # assert argument is valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem"),
    assertthat::is.count(min_restore),
    assertthat::noNA(min_restore),
    assertthat::is.count(max_restore),
    assertthat::noNA(max_restore),
    assertthat::is.number(min_proportion),
    assertthat::noNA(min_proportion),
    min_proportion >= 0,
    min_proportion <= 1,
    max_restore >= min_restore,
    unit %in% c("ha", "m", "km", "cells")
  )

  if (unit != "cells" && is.lonlat(problem$data$habitat_original)) {
    stop(paste("The input raster does not use a projected coordinate system.",
               "Please reproject, or use 'cell' as the unit measure for the",
               "restorable constraint."))
  }
  if (unit != "cells") {
    converted_area_min <- area_to_nb_cells(
      raster_layer = problem$data$habitat_original,
      area = min_restore,
      unit = unit
    )
    converted_area_max <- area_to_nb_cells(
      raster_layer = problem$data$habitat_original,
      area = max_restore,
      unit = unit
    )
  } else {
    converted_area_min <- min_restore
    converted_area_max <- max_restore
  }

  # add constraint
  add_restopt_constraint(
    problem = problem,
    constraint = restopt_component(
      name = paste0(
        "restorable (",
        "min_restore = ", as.integer(min_restore),
        ", max_restore = ", max_restore, ", unit = ", unit, ")"
      ),
      class = c("RestorableConstraint", "RestoptConstraint"),
      post = function(jproblem) {
        cell_area <- problem$data$cell_area
        rJava::.jcall(
          jproblem, "V", "postRestorableConstraint",
          as.integer(round(converted_area_min)), as.integer(round(converted_area_max)),
          .jarray(as.integer(as.vector(problem$data$cell_area))), min_proportion
        )
      }
    )
  )
}
