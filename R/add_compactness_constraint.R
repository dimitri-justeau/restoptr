#' @include internal.R
NULL

#' Add constraint to limit compactness
#'
#' Add constraint to a restoration problem ([restore_problem()] object
#' to specify the compactness of a solution.
#'
#' @param max_diameter `numeric` Maximum diameter value.
#'
#' @details
#' TODO. Provide details on how diameter is calculated and what sensible values
#' should be.
#'
#' @examples
#' # TODO
#'
#' @export
add_compactness_constraint <- function(x, max_diameter) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(x, "RestoptProblem"),
    assertthat::is.number(max_diameter),
    assertthat::noNA(max_diameter)
  )

  # throw warning if constraint specified
  i <- which(vapply(x$constraints, inherits, logical(1), "CompactnessConstraint"))
  if (length(i) > 0) {
    warning(
      "overwriting previously defined constraint.",
      call. = FALSE, immediate. = TRUE
    )
  } else {
    i <- length(x$constraints) + 1
  }

  # add constraints
  x$constraints[[i]] <- restopt_component(
    name = "Compactness constraint",
    data = list(max_diameter = max_diameter),
    post = function(jProblem) {
      .jcall(jProblem, "V", "postCompactnessConstraint", max_diameter)
    },
    class = c("RestoptObjective", "CompactnessConstraint")
  )

  # return object
  x
}
