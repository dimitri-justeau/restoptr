#' @include internal.R
NULL

#' Add constraint to specify the available amount of surface for restoration
#'
#' Add constraint to a restoration problem ([restore_problem()] object
#' to specify specify the available amount of surface for restoration
#'
#'
#' @param min_restore `integer` Minimum allowed area to restore in the solution.
#'
#' @param max_restore `integer` Maximum allowed area to restore in the solution
#'
#' @param cell_area `integer` Total area of a cell.
#'
#' @param min_proportion `float` Minimum habitat proportion to consider a cell
#' as restored.
#'
#' @details
#' TODO
#'
#' @examples
#' # TODO
#'
#' @export
add_restorable_constraint <- function(problem, min_restore, max_restore, cell_area,
                                      min_proportion = 1) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem"),
    assertthat::is.count(min_restore),
    assertthat::noNA(min_restore),
    assertthat::is.count(max_restore),
    assertthat::noNA(max_restore),
    assertthat::is.count(cell_area),
    assertthat::noNA(cell_area),
    assertthat::is.number(min_proportion),
    assertthat::noNA(min_proportion),
    min_proportion >= 0,
    min_proportion <= 1,
    max_restore >= min_restore,
    cell_area > 0
  )

  # add constraint
  add_restopt_constraint(
    problem = problem,
    constraint = restopt_component(
      name = "Restorable constraint",
      class = c("RestorableConstraint", "RestoptConstraint"),
      post = function(jproblem) {
        rJava::.jcall(
          jproblem, "V", "postRestorableConstraint",
          as.integer(min_restore), as.integer(max_restore), as.integer(cell_area), min_proportion
        )
      }
    )
  )
}
