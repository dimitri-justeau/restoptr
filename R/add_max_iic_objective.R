#' @include internal.R
NULL

#' Add an objective to maximize the integral index of connectivity
#'
#' Specify that a restoration problem ([restore_problem()] should
#' the integral index of connectivity (IIC).
#'
# @inheritParams add_max_mesh_objective
#'
#' @details
#' TODO. Provide details on what IIC is.
#'
#' @return An updated restoration problem ([restore_problem()] object.
#'
#' @examples
#' # TODO
#'
#' @export
add_max_mesh_iic <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, "RestoptProblem"))

  # add objective
  add_restopt_objective(
    x = x,
    objective = restopt_component(
      name = "Maximize integral index of connectivity",
      class = "MaxIicObjective",
      post = function(jproblem, precision, time_limit, output_path) {
        rJava::.jcall(
          jproblem, "Z", "maximizeIIC", precision, output_path, time_limit
        )
      }
    )
  )
}
