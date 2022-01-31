#' @include internal.R
NULL

#' Add locked out constraint
#'
#' Add constraint to a restoration problem ([restopt_problem()] object
#' to specify that certain planning units cannot be selected
#' for any restoration activities.
#'
#' @param data [terra::rast()] Raster object containing binary values
#'  that indicate which planning units cannot be selected for any restoration.
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

  src_locked_out <- basename(terra::sources(data)[[1]])

  add_restopt_constraint(
    problem = problem,
    constraint = restopt_component(
      name = paste0(
        "locked out (",
        "data = ", ifelse(src_locked_out != "", src_locked_out, "in memory"),
        ")"
      ),
      class = c("LockedOutConstraint", "RestoptConstraint"),
      post = function(jproblem) {} # nothing to do here
    )
  )
}
