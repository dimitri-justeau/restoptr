#' @include internal.R
NULL

#' Add constraint to limit the number of connected components
#'
#' Add constraint to a restoration problem ([restore_problem()] object
#' to specify the number of connected components that can be
#' present within a solution.
#'
#' @param min_nb_components `integer` Minimum number of connected components.
#'
#' @param max_nb_components `integer` Maximum number of connected components.
#'
#' @details
#' TODO. Provide details on what components are and what sensible values
#' should be.
#'
#' @examples
#' # TODO
#'
#' @export
add_components_constraint <- function(x, min_nb_components, max_nb_components) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(x, "RestoptProblem"),
    assertthat::is.count(min_nb_components),
    assertthat::noNA(min_nb_components),
    assertthat::is.count(max_nb_components),
    assertthat::noNA(max_nb_components)
  )

  # throw warning if constraint specified
  i <- which(
    vapply(x$constraints, inherits, logical(1), "ComponentConstraint")
  )
  if (length(i) > 0) {
    warning(
      "overwriting previously defined constraints.",
      call. = FALSE, immediate. = TRUE
    )
  } else {
    i <- length(x$constraints) + 1
  }

  # add constraints
  x$constraints[[i]] <- restopt_component(
    name = "Components constraint",
    data = list(min_nb_components = min_nb_components, max_nb_components = max_nb_components),
    post = function(jProblem) {
      .jcall(jProblem, "V", "postNbComponentsConstraint", min_nb_components, max_nb_components)
    },
    class = c("RestoptObjective", "ComponentConstraint")
  )

  # return object
  x
}
