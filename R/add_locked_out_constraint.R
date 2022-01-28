#' @include internal.R
NULL

#' Add locked out constraint
#'
#' Add constraint to a restoration problem ([restopt_problem()] object
#' to specify that certain planning units cannot be selected
#' for any restoration activities.
#'
#' @inheritParams add_max_mesh_objective
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
#' \dontrun{TODO}
#'
#' @export
add_locked_out_constraint <- function(problem, data) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem"),
    inherits(data, "SpatRaster")
  )
  ## further checks
  assertthat::assert_that(
    terra::hasValues(data)
  )
  assertthat::assert_that(
    terra::compareGeom(
      problem$data$existing_habitat, data, stopiffalse = FALSE
    ),
    msg = paste(
      "argument to \"data\" has different spatial properties to",
      "the \"existing_habitat\" and \"restorable_habitat\" data in",
      "the problem"
    )
  )

  problem$data$locked_out <- list(
    data = data
  )

  add_restopt_constraint(
    problem = problem,
    constraint = restopt_component(
      name = paste0(
        "locked out (",
        "data = ", basename(terra::sources(data)$source[[1]])
      ),
      class = c("LockedOutConstraint", "RestoptConstraint"),
      post = function(jproblem) {} # nothing to do here
    )
  )
}
