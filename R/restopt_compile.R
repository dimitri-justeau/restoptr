#' @include internal.R
NULL

#' Compile a restoration optimization problem
#'
#' Compile a restoration optimization problem ([resopt_problem()])
#' for optimization.
#'
# @inheritParams add_max_mesh_objective
#'
#' @return A Java reference (`.jobjRef`) object.
#'
#' @examples
#' # TODO
#'
#' @noRd
restopt_compile <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, "RestoptProblem"))
  # TODO
}
