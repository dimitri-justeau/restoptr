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

  # throw warning if objective specified
  if (!is.null(x$objective)) {
    warning(
      "overwriting previously defined objective.",
      call. = FALSE, immediate. = TRUE
    )
  }

  # add objective
  x$objective <- restopt_component(
    name = "Maximize integral index of connectivity",
    java = "maximizeIIC",
    class = c("RestoptObjective", "MaxIicObjective")
  )

  # return object
  x
}
