#' @include internal.R
NULL

#' Add locked out constraint
#'
#' Add constraint to a restoration problem ([restore_problem()] object
#' to specify that certain planning units cannot be selected
#' for any restoration activities.
#'
#' @param data [terra::rast()] Raster object containing binary values
#'  that indicate which planning units cannot be selected for any restoration.
#'
#' @param raster_value Raster value of cells to lock out (or not to lock out,
#'  see `lock_out` parameter)
#'
#' @param lock_out If TRUE cell with value `raster_value` in `data` are locked
#'  out, otherwise cells with values different from `raster_value` are locked
#'  out.
#'
#' @details
#' TODO.
#'
#' @examples
#' # TODO
#'
#' @export
add_locked_out_constraint <- function(x, data, raster_value = 1, lock_out = FALSE) {
  # assert argument is valid
  ## Check locked_out_raster_value and available_raster_value
  assertthat::is.flag(lock_out)
  assertthat::noNA(raster_value)
  assertthat::is.count(raster_value)
  ##
  assertthat::assert_that(
    inherits(x, "RestoptProblem"),
    inherits(data, "SpatRaster")
  )
  ## further checks
  assertthat::assert_that(
    terra::hasValues(data)
  )
  assertthat::assert_that(
    terra::compareGeom(x$data$existing_habitat, data, stopiffalse = FALSE),
    msg = paste(
      "argument to \"data\" has different spatial properties to",
      "the \"existing_habitat\" and \"restorable_habitat\" data in",
      "the problem"
    )
  )

  # throw warning if constraint specified
  i <- which(
    vapply(x$constraints, inherits, logical(1), "LockedOutConstraint")
  )
  if (length(i) > 0) {
    warning(
      "overwriting previously defined constraint.",
      call. = FALSE, immediate. = TRUE
    )
  } else {
    i <- length(x$constraints) + 1
  }

  x$constraints[[i]] <- restopt_component(
    name = "Locked out constraint",
    data = list(data = data, raster_value = raster_value, lock_out = lock_out),
    post = function() {}, # Nothing to do here
    class = c("RestoptConstraint", "LockedOutConstraint")
  )

  x$data$locked_out <- data

  # return object
  x
}