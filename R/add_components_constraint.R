#' @include internal.R
NULL

#' Add constraint to limit the number of connected components
#'
#' Add constraint to a restoration problem ([restopt_problem()]) object
#' to specify the number of connected components that can be
#' present within a solution.
#'
#' @inheritParams set_max_mesh_objective
#'
#' @param min_nb_components `integer` Minimum number of connected components.
#'
#' @param max_nb_components `integer` Maximum number of connected components.
#'
#' @details A connected component is a spatially continuous set of planning
#' units. This constraints applies on the set of planning units that are
#' selected for restoration, and allows to specify a minimum and maximum
#' number of connected components. In practice, this constraint is useful
#' to ensure the feasibility of a restoration project, and to integrate
#' economies of scale. Continuous restoration areas (i.e. less connected
#' components) are usually associated with lower costs, because it ensures that
#' restoration sites are not too far away from each other (e.g. lower travel
#' costs between sites, less areas to monitor, etc.). On the other hand,
#' it can be useful to enforce several disconnected restoration areas to
#' ensure that hazards (e.g. fire) do not strike all planning units at the
#' same time.
#' Also see \link{add_compactness_constraint}.
#'
#' @seealso
#' \link{add_compactness_constraint}
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
#' add_components_constraint(1, 1)
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
add_components_constraint <- function(problem,
                                      min_nb_components,
                                      max_nb_components) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem"),
    assertthat::is.count(min_nb_components),
    assertthat::noNA(min_nb_components),
    assertthat::is.count(max_nb_components),
    assertthat::noNA(max_nb_components)
  )

  # coerce arguments to integer
  min_nb_components <- as.integer(min_nb_components)
  max_nb_components <- as.integer(max_nb_components)

  # add constraint
  add_restopt_constraint(
    problem = problem,
    constraint = restopt_component(
      name = paste0(
        "components (",
        "min_nb_components = ", min_nb_components,
        ", max_nb_components = ", max_nb_components, ")"
      ),
      class = c("ComponentConstraint", "RestoptConstraint"),
      post = function(jproblem) {
        rJava::.jcall(
          jproblem, "V", "postNbComponentsConstraint",
          min_nb_components, max_nb_components
        )
      }
    )
  )
}
