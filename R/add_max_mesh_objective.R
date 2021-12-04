#' @include internal.R
NULL

#' Add an objective to maximize effective mesh size
#'
#' Specify that a restoration problem ([restore_problem()] should
#' maximize effective mesh size.
#'
#' @param x [restore_problem()] Restoration problem object.
#'
#' @details
#' TODO. Provide details on what effective mesh size is.
#'
#' @return An updated restoration problem ([restore_problem()] object.
#'
#' @references
#' TODO. Citation for effective mesh size.
#'
#' @examples
#' # TODO
#'
#' @export
add_max_mesh_objective <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, "RestoptProblem"))

  # add objective
  add_restopt_objective(
    x = x,
    objective = restopt_component(
      name = "Maximize effective mesh size",
      class = "MaxMeshObjective",
      post = function(jproblem, precision, time_limit, output_path) {
        rJava::.jcall(
          jproblem, "Z", "maximizeMESH", precision, output_path, time_limit
        )
      }
    )
  )
}
