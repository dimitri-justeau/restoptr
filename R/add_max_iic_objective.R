#' @include internal.R
NULL

#' Add an objective to maximize the integral index of connectivity
#'
#' Specify that a restoration problem ([restopt_problem()] should
#' the integral index of connectivity (IIC).
#'
#' @inheritParams add_max_mesh_objective
#'
#' @details
#' TODO. Provide details on what IIC is.
#'
#' @return An updated restoration problem ([restopt_problem()] object.
#'
#' @examples
#' \dontrun{TODO}
#'
#' @export
add_max_iic_objective <- function(problem) {
  # assert argument is valid
  assertthat::assert_that(inherits(problem, "RestoptProblem"))

  # add objective
  add_restopt_objective(
    problem = problem,
    objective = restopt_component(
      name = "Maximize integral index of connectivity",
      class = c("MaxIicObjective", "RestoptObjectve"),
      post = function(jproblem, precision, time_limit, output_path) {
        rJava::.jcall(
          jproblem, "Z", "maximizeIIC", precision, output_path, time_limit
        )
      }
    )
  )
}
