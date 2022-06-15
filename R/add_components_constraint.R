#' @include internal.R
NULL

#' Add constraint to limit the number of connected components
#'
#' Add constraint to a restoration problem ([restopt_problem()]) object
#' to specify the number of connected components that can be
#' present within a solution.
#'
#' @inheritParams set_max_mesh_objective
#' @inherit set_max_mesh_objective return
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
#'
#' @family constraints
#'
#' @examples
#' \donttest{
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
#'     min_restore = 10,
#'     max_restore = 100,
#'   ) %>%
#'   add_components_constraint(1, 1)
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
add_components_constraint <- function(problem,
                                      min_nb_components,
                                      max_nb_components) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem"),
    assertthat::is.count(min_nb_components),
    assertthat::noNA(min_nb_components),
    assertthat::is.count(max_nb_components),
    assertthat::noNA(max_nb_components),
    isTRUE(max_nb_components >= min_nb_components)
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
