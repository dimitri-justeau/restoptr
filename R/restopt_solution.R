#' @include internal.R
NULL

## Define RestoptSolution class as inheriting SpatRaster
setClass("RestoptSolution", contains = "SpatRaster", slots = representation(metadata="list", problem="ANY"))

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
  solution@problem <- problem
  return(solution)
}

#'
#' Restopt solution metadata
#'
#' Return the metadata associated with a restopt solution, which contains
#' the characteristics of the solution. The unit for area characteristics
#' can be chosen among "ha" (hectares), "m" (square meters), "km" (square
#' kilometers), and "cells" (cells from the original input habitat raster).
#' Note that the solving time is expressed in seconds.
#'
#' @param restopt_solution [`RestoptSolution`] Restopt solution
#' to this solution.
#'
#' @return A list containing the characteristics of the restopt solution.
#'
#' @export
get_metadata <- function(restopt_solution, area_unit = "ha") {
  assertthat::assert_that(
    inherits(restopt_solution, "RestoptSolution"),
    area_unit %in% c("ha", "m", "km", "cells")
  )
  metadata <- restopt_solution@metadata
  original_habitat <- restopt_solution@problem$data$habitat_original
  min_rest <- as.numeric(metadata$min_restore)
  max_rest <- as.numeric(metadata$total_restorable)
  if (area_unit == "ha") {
    metadata$min_restore <- nb_cell_to_area(original_habitat, min_rest, "ha")
    metadata$total_restorable <- nb_cell_to_area(original_habitat, max_rest, "ha")
    if ("mesh_initial" %in% names(metadata)) {
      metadata$mesh_initial <- nb_cell_to_area(original_habitat, metadata$mesh_initial, "ha")
      metadata$mesh_best <- nb_cell_to_area(original_habitat, metadata$mesh_best, "ha")
    }
  }
  if (area_unit == "m") {
    metadata$min_restore <- nb_cell_to_area(original_habitat, min_rest, "m")
    metadata$total_restorable <- nb_cell_to_area(original_habitat, max_rest, "m")
    if ("mesh_initial" %in% names(metadata)) {
      metadata$mesh_initial <- nb_cell_to_area(original_habitat, metadata$mesh_initial, "m")
      metadata$mesh_best <- nb_cell_to_area(original_habitat, metadata$mesh_best, "m")
    }
  }
  if (area_unit == "km") {
    metadata$min_restore <- nb_cell_to_area(original_habitat, min_rest, "km")
    metadata$total_restorable <- nb_cell_to_area(original_habitat, max_rest, "km")
    if ("mesh_initial" %in% names(metadata)) {
      metadata$mesh_initial <- nb_cell_to_area(original_habitat, metadata$mesh_initial, "km")
      metadata$mesh_best <- nb_cell_to_area(original_habitat, metadata$mesh_best, "km")
    }
  }
  return(metadata)
}
