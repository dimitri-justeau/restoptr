#' @include internal.R
NULL

#' Add constraint to limit compactness
#'
#' Add constraint to a restoration problem ([restore_problem()] object
#' to specify the compactness of a solution.
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
add_compactness_constraint <- function(x, max_diameter) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(x, "RestoptProblem"),
    assertthat::is.number(max_diameter),
    assertthat::noNA(max_diameter)
  )

  # add constraint
  add_restopt_constraint(
    x = x,
    objective = restopt_component(
      name = "Compactness constraint",
      class = "CompactnessConstraint",
      post = function(jproblem) {
        rJava::.jcall(jproblem, "V", "postCompactnessConstraint", max_diameter)
      }
    )
  )
}
