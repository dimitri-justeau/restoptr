#' @include internal.R
NULL

#' Add constraint to ensure that the selected planning units for restoration
#' are connected.
#'
#' Add constraint to a restoration problem ([restopt_problem()]) object
#' to specify the selected planning units are connected
#'
#' @inheritParams add_max_mesh_objective
#'
#'
#' @details A connected area is such that there is a path between any to
#' planning units within the area. This constraints applies on the set of
#' planning units that are selected for restoration. In practice, this
#' constraint is useful to ensure the feasibility of a restoration project,
#' and to integrate economies of scale. Connected restoration areas are
#' usually associated with lower costs, because it ensures that restoration
#' sites are not too far away from each other (e.g. lower travel
#' costs between sites, less areas to monitor, etc.). **Note** This constraint
#' relies on the add_components_constraint(), with parameters set to enforce
#' exactly one connected component. Also see \link{add_components_constraint}
#' and \link{add_compactness_constraint}.
#'
#' @seealso
#' \link{add_compactness_constraint}, \link{add_components_constraint}
#'
#' @examples
#' \dontrun{
#' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat.tif", package = "restoptr")
#' )
#'
#' restorable_data <- rast(
#'   system.file("extdata", "restorable.tif", package = "restoptr")
#' )
#'
#' # plot data
#' plot(rast(list(habitat_data, restorable_data)), nc = 2)
#'
#' # create problem
#' p <- restopt_problem(
#'        existing_habitat = habitat_data,
#'        restorable_habitat = restorable_data
#' ) %>%
#' add_restorable_constraint(
#'   min_restore = 10,
#'   max_restore = 100,
#'   cell_area = 23
#' ) %>%
#' add_connected_constraint()
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
add_connected_constraint <- function(problem) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem")
  )
  add_components_constraint(problem, 1, 1)
}
