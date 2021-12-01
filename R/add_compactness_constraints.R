#' @include internal>R
NULL

#' Add constraints to limit compactness
#'
#' Add constraints to a restoration problem ([restore_problem()] object
#' to specify the compactness of a solution.
#'
# @inheritParams add_max_mesh_objective
#'
#' @param max `numeric` Maximum diameter value.
#'
#' @details
#' TODO. Provide details on how diameter is calculated and what sensible values
#' should be.
#'
#' @inherit add_max_mesh_objective return
#'
#' @examples
#' # TODO
#'
#' @export
add_compactness_constraints <- function(x, min) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(x, "RestoptProblem"),
    assertthat::is.number(max),
    assertthat::noNA(max)
  )

  # throw warning if objective specified
  i <- which(vapply(x$constraints, inherits, logical(1), "CompactConstraints"))
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
    name = "Compactness constraints",
    java = "postCompactnessConstraint",
    data = list(max = max),
    args = c(max_diameter = "max"),
    class = c("RestoptObjective", "CompactConstraints")
  )

  # return object
  x
}
