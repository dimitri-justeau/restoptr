#' @include internal.R
NULL

#' Add constraint to limit compactness
#'
#' Add constraint to a restoration problem ([restopt_problem()]) object
#' to specify the compactness of a solution.
#'
#' @inheritParams set_max_mesh_objective
#' @inherit set_max_mesh_objective return
#'
#' @param max_diameter `numeric` Maximum diameter value.
#'
#' @param unit `unit` object or a `character` that can be coerced to an area
#' unit (see `unit` package), or "cells" for cell width of aggregated
#' habitat raster. Corresponds to the unit of the maximum diameter. If the
#' input habitat raster does not use a projected coordinate system, only "cells"
#' is available.
#'
#' @details The compactness constraint is defined according to the diameter of
#' the smallest enclosing circle which contains the center of selected planning
#' units for restoration (see https://w.wiki/4vfg). The unit of the diameter
#' corresponds either to a unit available in the `unit` package, or to planning
#' unit width ("cells"). Note that, as the computation occurs on aggregated cells,
#' if `max_diameter` is used with a different unit than "cells", it will be rounded
#' to the closest corresponding number of cells. For example, a diameter of 4 cells
#' means that no more than 4 cells can be found in line in the solution. In practice,
#' this constraint is useful to ensure the feasibility of a restoration project,
#' and to integrate economies of scale. Compact restoration areas are usually
#' associated with lower costs and easier management, because it ensures that
#' restoration sites are not too far away from each other (e.g. lower travel costs
#' between sites, less areas to monitor, etc.).
#'
#' @family constraints
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
#'   add_compactness_constraint(1800, unit = "m")
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
add_compactness_constraint <- function(problem, max_diameter, unit = "m") {
  # assert argument is valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem"),
    assertthat::is.number(max_diameter),
    assertthat::noNA(max_diameter),
    isTRUE(max_diameter > 0),
    (unit == "cells" || units::ud_are_convertible(unit, "m"))
  )
  max_diameter_unitless <- max_diameter
  if (unit != "cells") {
    width <- cell_width(get_existing_habitat(problem), unit = unit)
    max_diameter_unitless <- max_diameter / width
  }

  # add constraint
  add_restopt_constraint(
    problem = problem,
    constraint = restopt_component(
      name = paste0("compactness (max_diameter = ", max_diameter, ", unit = ", unit, ")"),
      class = c("CompactnessConstraint", "RestoptConstraint"),
      post = function(jproblem) {
        rJava::.jcall(jproblem, "V", "postCompactnessConstraint", max_diameter_unitless)
      }
    )
  )
}
