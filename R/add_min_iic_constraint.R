#' @include internal.R
NULL

#' Add constraint to enforce a minimum integral index of connectivity (IIC) value
#'
#' Add constraint to a restoration problem ([restopt_problem()]) object
#' to specify the minimum integral index of connectivity of a solution.
#'
#' @inheritParams set_max_mesh_objective
#' @inherit set_max_mesh_objective return
#'
#' @param min_iic `numeric` Minimum IIC value (between 0 and 1).
#'
#' @param distance_threshold `numeric` greater than 0. Minimum distance (in
#' `unit`) between two patches to consider them connected in the computation of
#' the IIC. The default value -1 causes the function to use 1 aggregated cell as
#' the distance threshold.
#'
#' @param unit `unit` object or a `character` that can be coerced to a distance
#' unit (see `unit` package), or "cells" for cell width of aggregated
#' habitat raster. Units of the `distance_threshold` parameter. If the input
#' habitat raster does not use a projected coordinate system, only "cells" is
#' available. Meters by default, expected if `distance_threshold` is set to its
#' default value (-1), which causes the function to use 1 cell by default.
#'
#' @param precision `integer` Precision for calculations.
#' Defaults to 4.
#'
#' @details The integral index of connectivity (IIC) is a graph-based inter-patch
#' connectivity index based on a binary connection model (Pascual-Hortal &
#' Saura, 2006). Its maximization in the context of restoration favours
#' restoring the structural connectivity between large patches. IIC is unitless
#' and comprised between 0 (no connectivity) and 1 (all the landscape is
#' habitat, thus fully connected). The `distance_threshold` parameter indicates
#' to the solver how to construct the habitat graph, i.e. what is the minimum
#' distance between two patches to consider them as connected. Note that, as
#' the computation occurs on aggregated cells, if `distance_threshold` is used
#' with a different unit than "cells", it will be rounded to the closest
#' corresponding number of cells.
#'
#' @details The effective mesh size (MESH) is a measure of landscape fragmentation
#' based on the probability that two randomly chosen points are located in the
#' same patch (Jaeger, 2000). Maximizing it in the context of restoration
#' favours fewer and larger patches.
#'
#' @family constraints
#'
#' @seealso
#' \link{set_max_iic_objective}
#'
#' @references
#' Pascual-Hortal, L., & Saura, S. (2006).
#' Comparison and development of new graph-based landscape connectivity indices:
#' Towards the priorization of habitat patches and corridors for conservation.
#' Landscape Ecology, 21(7), 959‑967. https://doi.org/10.1007/s10980-006-0013-z
#'
#' @examples
#' \donttest{
#' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#'
#' locked_out_data <- rast(
#'  system.file("extdata", "locked_out.tif", package = "restoptr")
#' )
#'
#' # create problem with locked out constraints
#' p <- restopt_problem(
#'     existing_habitat = habitat_data,
#'     aggregation_factor = 16,
#'     habitat_threshold = 0.7
#'   ) %>%
#'   add_min_iic_constraint(0.2) %>%
#'   add_restorable_constraint(
#'     min_restore = 200,
#'     max_restore = 300,
#'   ) %>%
#'   add_locked_out_constraint(data = locked_out_data) %>%
#'   add_compactness_constraint(2500, unit = "m") %>%
#'   add_settings(time_limit = 1)
#'
#' # print problem
#' print(p)
#'
#' # solve problem
#' s <- solve(p)
#'
#' # plot solution
#' plot(s)
#' }
#'
#' @export
add_min_iic_constraint <- function(problem, min_iic, distance_threshold = -1,
                                   unit = "m", precision = 4) {
  if (distance_threshold < 0) {
    distance_threshold <- 1
    unit <- "cells"
  }
  # assert argument is valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem"),
    assertthat::is.number(min_iic),
    min_iic >= 0 && min_iic <= 1,
    assertthat::is.count(precision),
    assertthat::noNA(precision),
    assertthat::is.count(distance_threshold),
    assertthat::noNA(distance_threshold),
    (unit == "cells" || units::ud_are_convertible(unit, "m"))
  )
  distance_threshold_unitless <- distance_threshold
  if (unit != "cells") {
    width <- cell_width(get_existing_habitat(problem), unit = unit)
    distance_threshold_unitless <- distance_threshold / width
  }
  distance_threshold_unitless <- as.integer(round(distance_threshold_unitless))
  if (distance_threshold_unitless == 0) {
    warning(paste0("The distance threshold was rounded to 1 aggregated cell (",
                   width, ") because it was less than this minimum value"))
    distance_threshold_unitless <- 1L
  }

  # add constraint
  add_restopt_constraint(
    problem = problem,
    constraint = restopt_component(
      name = paste0("min IIC (min_iic = ", min_iic, ", distance_threshold = ",
                    distance_threshold, ", unit = ", unit, ", precision = ",
                    precision, ")"),
      class = c("MinIICConstraint", "RestoptConstraint"),
      post = function(jproblem) {
        rJava::.jcall(jproblem, "V", "postMinIICConstraint", min_iic, distance_threshold_unitless, as.integer(precision))
      }
    )
  )
}
