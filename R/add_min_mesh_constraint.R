#' @include internal.R
NULL

#' Add constraint to enforce a minimum effective mesh size (MESH) value
#'
#' Add constraint to a restoration problem ([restopt_problem()]) object
#' to specify the minimum effective mesh size of a solution.
#'
#' @inheritParams set_max_mesh_objective
#' @inherit set_max_mesh_objective return
#'
#' @param min_mesh `numeric` Minimum MESH value.
#'
#' @param precision `integer` Precision for calculations.
#' Defaults to 4.
#'
#' @param unit `unit` object or a `character` that can be coerced to an area
#' unit (see `unit` package), or "cells" for cell width of aggregated
#' habitat raster. Corresponds to the unit of the minimum mesh value If the
#' input habitat raster does not use a projected coordinate system, only "cells"
#' is available. Defaults to "ha".
#'
#' @details The effective mesh size (MESH) is a measure of landscape fragmentation
#' based on the probability that two randomly chosen points are located in the
#' same patch (Jaeger, 2000). Maximizing it in the context of restoration
#' favours fewer and larger patches.
#'
#' @family constraints
#'
#' @seealso
#' \link{set_max_mesh_objective}
#'
#' @references
#' Jaeger, J. A. G. (2000). Landscape division, splitting index, and effective
#' mesh size: New measures of landscape fragmentation. Landscape Ecology, 15(2),
#' 115‑130. https://doi.org/10.1023/A:1008129329289
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
#'   ) %>%
#'   add_min_mesh_constraint(min_mesh = 2500, unit = "ha")
#'
#' # plot preprocessed data
#' plot(rast(list(p$data$existing_habitat, p$data$restorable_habitat)), nc = 2)
#'
#' # print problem
#' print(p)
#'
#' # Solve problem
#' s <- solve(p)
#' # plot solution
#' plot(s)
#' }
#'
#' @export
add_min_mesh_constraint <- function(problem, min_mesh, precision = 4, unit = "ha") {
  # assert argument is valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem"),
    assertthat::is.number(min_mesh),
    assertthat::noNA(min_mesh),
    assertthat::is.count(precision),
    assertthat::noNA(precision),
    isTRUE(min_mesh > 0),
    (unit == "cells" || units::ud_are_convertible(unit, "ha"))
  )
  min_mesh_unitless <- min_mesh
  if (unit != "cells") {
    min_mesh_unitless <- area_to_nb_cells(
      get_existing_habitat(problem),
      area = min_mesh,
      unit = unit
    )
  }

  # add constraint
  add_restopt_constraint(
    problem = problem,
    constraint = restopt_component(
      name = paste0("min MESH (min_mesh = ", min_mesh, ", precision = ", precision, ", unit = ", unit, ")"),
      class = c("MinMeshConstraint", "RestoptConstraint"),
      post = function(jproblem) {
        rJava::.jcall(jproblem, "V", "postMinMeshConstraint", min_mesh_unitless, as.integer(precision))
      }
    )
  )
}
