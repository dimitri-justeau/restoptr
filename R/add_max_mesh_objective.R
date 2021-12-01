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

  # throw warning if objective specified
  if (!is.null(x$objective)) {
    warning(
      "overwriting previously defined objective.",
      call. = FALSE, immediate. = TRUE
    )
  }

  # add objective
  x$objective <- restopt_component(
    name = "Maximize effective mesh size",
    java = "maximizeMESH",
    class = c("RestoptObjective", "MaxMeshObjective")
  )

  # return object
  x
}
