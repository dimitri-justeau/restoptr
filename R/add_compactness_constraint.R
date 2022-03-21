#' @include internal.R
NULL

#' Add constraint to limit compactness
#'
#' Add constraint to a restoration problem ([restopt_problem()]) object
#' to specify the compactness of a solution.
#'
#' @inheritParams set_max_mesh_objective
#'
#' @param max_diameter `numeric` Maximum diameter value.
#'
#' @details The compactness constraint is defined according to the diameter of
#' the smallest enclosing circle which contains the center of selected planning
#' units for restoration (see https://w.wiki/4vfg). The unit of the diameter
#' corresponds to planning unit width. For example, a diameter of 4 means that
#' no more than 4 cells can be found in line in the solution. In practice,
#' this constraint is useful to ensure the feasibility of a restoration project,
#' and to integrate economies of scale. Compact restoration areas are usually
#' associated with lower costs and easier management, because it ensures that
#' restoration sites are not too far away from each other (e.g. lower travel
#' costs between sites, less areas to monitor, etc.).
#'#' Also see \link{add_components_constraint}.
#'
#' @seealso
#' \link{add_components_constraint}
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
#'   add_compactness_constraint(4)
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
#' plot(
#'   x = s, main = "solution",
#'   col = c("#E5E5E5", "#fff1d6", "#b2df8a", "#1f78b4")
#' )
#' }
#'
#' @export
add_compactness_constraint <- function(problem, max_diameter) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem"),
    assertthat::is.number(max_diameter),
    assertthat::noNA(max_diameter)
  )

  # add constraint
  add_restopt_constraint(
    problem = problem,
    constraint = restopt_component(
      name = paste0("compactness (max_diameter = ", max_diameter, ")"),
      class = c("CompactnessConstraint", "RestoptConstraint"),
      post = function(jproblem) {
        rJava::.jcall(jproblem, "V", "postCompactnessConstraint", max_diameter)
      }
    )
  )
}
