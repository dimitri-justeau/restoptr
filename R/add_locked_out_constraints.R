#' @include internal>R
NULL

#' Add locked out constraints
#'
#' Add constraint to a restoration problem ([restore_problem()] object
#' to specify that certain planning units cannot be selected
#' for any restoration activities.
#'
# @inheritParams add_max_mesh_objective
#'
#' @param data [terra::rast()] Raster object containing binary values
#'  that indicate which planning units cannot be selected for any restoration.
#'
#' @details
#' TODO.
#'
#' @inherit add_max_mesh_objective return
#'
#' @examples
#' # TODO
#'
#' @export
add_locked_out_constraints <- function(x, data) {
  # assert argument is valid
  ##
  assertthat::assert_that(
    inherits(x, "RestoptProblem"),
    inherits(data, "SpatRaster")
  )
  ## further checks
  assertthat::assert_that(
    terra::has_values(data)
  )
  assertthat::assert_that(
    terra::compareGeom(existing_habitat, data, stopiffalse = FALSE),
    msg = paste(
      "argument to \"data\" has different spatial properties to",
      "the \"existing_habitat\" and \"restorable_habitat\" data in",
      "the problem"
    )
  )

  # throw warning if objective specified
  i <- which(
    vapply(x$constraints, inherits, logical(1), "LockedOutConstraints")
  )
  if (length(which(i)) > 0) {
    warning(
      "overwriting previously defined constraints.",
      call. = FALSE, immediate. = TRUE
    )
  } else {
    i <- length(x$constraints) + 1
  }

  # TODO: insert logic for adding constraints

  # return object
  x
}
