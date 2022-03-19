#' @include internal.R
NULL

#' Define RestoptSolution class as inheriting SpatRaster
setClass("RestoptSolution", contains = "SpatRaster", slots = representation(metadata="list"))

#' Restopt solution
#'
#' An object representing a restopt problem solution. It is basically a
#' SpatRaster with a few metadata attributes added.
#'
#' @param restopt_problem [`RestoptProblem`] Reference to the problem corresponding
#' to this solution.
#'
#' @param solution_raster [terra::rast()] Solution raster.
#'
#' @param metadata [`list`] List containing metadata attributes of the solution.
#'
#' @return A new restoration problem solution (`RestoptSolution`).
#'
restopt_solution <- function(restopt_problem, solution_raster, metadata) {
  # assert arguments are valid
  ## initial checks
  assertthat::assert_that(
    inherits(solution_raster, "SpatRaster"),
    inherits(problem, "RestoptProblem"),
    is.list(metadata)
  )
  ## further checks
  assertthat::assert_that(
    terra::hasValues(solution_raster),
    terra::nlyr(solution_raster) == 1,
    terra::compareGeom(
      solution_raster, restopt_problem$data$existing_habitat, stopiffalse = FALSE
    )
  )
  # convert object to RestoptSolution
  solution <- as(solution_raster, "RestoptSolution")
  solution@metadata <- metadata
  return(solution)
}
