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
#' @param min_proportion `float` Minimum habitat proportion to consider a cell as restored.
#'
#' @details
#' TODO
#'
#' @examples
#' # TODO
#'
#' @export
add_restorable_constraint <- function(x, min_restore, max_restore, cell_area, min_proportion = 1) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(x, "RestoptProblem"),
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

  # throw warning if constraint specified
  i <- which(
    vapply(x$constraints, inherits, logical(1), "RestorableConstraint")
  )
  if (length(i) > 0) {
    warning(
      "overwriting previously defined constraints.",
      call. = FALSE, immediate. = TRUE
    )
  } else {
    i <- length(x$constraints) + 1
  }

  # add constraints
  x$constraints[[i]] <- restopt_component(
    name = "Components constraint",
    data = list(
      min_restore = min_restore,
      max_restore = max_restore,
      cell_area = cell_area,
      min_proportion = min_proportion
    ),
    post = function(jProblem) {
      .jcall(jProblem, "V", "postRestorableConstraint", min_restore, max_restore, cell_area, min_proportion)
    },
    class = c("RestoptObjective", "RestorableConstraint")
  )

  # return object
  x
}
