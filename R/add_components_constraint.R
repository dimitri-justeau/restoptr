#' @include internal.R
NULL

#' Add constraint to limit the number of connected components
#'
#' Add constraint to a restoration problem ([restore_problem()] object
#' to specify the number of connected components that can be
#' present within a solution.
#'
#' @param min_nb_components `integer` Minimum number of connected components.
#'
#' @param max_nb_components `integer` Maximum number of connected components.
#'
#' @details
#' TODO. Provide details on what components are and what sensible values
#' should be.
#'
#' @examples
#' # TODO
#'
#' @export
add_components_constraint <- function(problem, min_nb_components, max_nb_components) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem"),
    assertthat::is.count(min_nb_components),
    assertthat::noNA(min_nb_components),
    assertthat::is.count(max_nb_components),
    assertthat::noNA(max_nb_components)
  )

  # add constraint
  add_restopt_constraint(
    problem = problem,
    constraint = restopt_component(
      name = "Components constraint",
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
