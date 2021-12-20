#' @include internal.R
NULL

#' Add an objective to maximize effective mesh size
#'
#' Specify that a restoration problem ([restopt_problem()] should
#' maximize effective mesh size.
#'
#' @param problem [restopt_problem()] Restoration problem object.
#'
#' @details
#' TODO. Provide details on what effective mesh size is.
#'
#' @return An updated restoration problem ([restopt_problem()] object.
#'
#' @references
#' TODO. Citation for effective mesh size.
#'
#' @examples
#' \dontrun{TODO}
#'
#' @export
add_max_mesh_objective <- function(problem) {
  # assert argument is valid
  assertthat::assert_that(inherits(problem, "RestoptProblem"))

  # add objective
  add_restopt_objective(
    problem = problem,
    objective = restopt_component(
      name = "Maximize effective mesh size",
      class = c("MaxMeshObjective", "RestoptObjectve"),
      post = function(jproblem, precision, time_limit, output_path) {
        rJava::.jcall(
          jproblem, "Z", "maximizeMESH", precision, output_path, time_limit
        )
      }
    )
  )
}
