#' @include internal
NULL

#' Solve a restoration optimization problem
#'
#' Solve a restoration optimization problem to generate a solution.
#'
# @inheritParams add_max_mesh_objective
#'
#' @return A [terra::rast()] object.
#'
#' @examples
#' # TODO
#'
#' @export
solve.RestoptProblem <- function(a, b, ...) {
  # assert argument is valid
  assertthat::assert_that(inherits(a, "RestoptProblem"))

  # compile problem
  o <- restopt_compile(a)

  # solve problem
  # TODO

  # prepare result
  # TODO

  # return result
  # TODO
}
