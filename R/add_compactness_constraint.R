#' @include internal.R
NULL

#' Add constraint to limit compactness
#'
#' Add constraint to a restoration problem ([restopt_problem()] object
#' to specify the compactness of a solution.
#'
#' @inheritParams add_max_mesh_objective
#'
#' @param max_diameter `numeric` Maximum diameter value.
#'
#' @details
#' TODO. Provide details on how diameter is calculated and what sensible values
#' should be.
#'
#' @examples
#' # TODO
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
