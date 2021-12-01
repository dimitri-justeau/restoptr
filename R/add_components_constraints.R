#' @include internal>R
NULL

#' Add constraints to limit the number of connected components
#'
#' Add constraints to a restoration problem ([restore_problem()] object
#' to specify the number of connected components that can be
#' present within a solution.
#'
# @inheritParams add_max_mesh_objective
#'
#' @param min `integer` Minimum number of connected components.
#'
#' @param max `integer` Maximum number of connected components.
#'
#' @details
#' TODO. Provide details on what components are and what sensible values
#' should be.
#'
#' @inherit add_max_mesh_objective return
#'
#' @examples
#' # TODO
#'
#' @export
add_components_constraints <- function(x, min, max) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(x, "RestoptProblem"),
    assertthat::is.count(min),
    assertthat::noNA(min),
    assertthat::is.count(max),
    assertthat::noNA(max)
  )

  # throw warning if objective specified
  i <- which(
    vapply(x$constraints, inherits, logical(1), "ComponentConstraints")
  )
  if (length(which(i)) > 0) {
    warning(
      "overwriting previously defined constraints.",
      call. = FALSE, immediate. = TRUE
    )
  } else {
    i <- length(x$constraints) + 1
  }

  # add constraints
  x$constraints[[i]] <- restopt_component(
    name = "Components constraints",
    java = "postNbComponentsConstraint",
    data = list(min = min, max = max),
    args = c(min_nb_components = "min", max_nb_components = "max"),
    class = c("RestoptObjective", "ComponentConstraints")
  )

  # return object
  x
}
