#' @include internal.R
NULL

## Define RestoptSolution class as inheriting SpatRaster
setClass("RestoptSolution", contains = "SpatRaster", slots = representation(metadata="list", problem="ANY"))

#' Restopt solution
#'
#' An object representing a restopt problem solution. It is basically a
#' SpatRaster with a few metadata attributes added.
#'
#' @param restopt_problem [`restopt_problem()`] Reference to the problem corresponding
#' to this solution.
#'
#' @param solution_raster [terra::rast()] Solution raster.
#'
#' @param metadata [`list`] List containing metadata attributes of the solution.
#'
#' @param id_solution [`integer`] Identifier of the solution.
#'
#' @return A new restoration problem solution (`restopt_solution()`).
#'
#' @importFrom methods as
#'
restopt_solution <- function(restopt_problem, solution_raster, metadata, id_solution = 1) {
  # assert arguments are valid
  ## initial checks
  assertthat::assert_that(
    inherits(solution_raster, "SpatRaster"),
    inherits(restopt_problem, "RestoptProblem"),
    is.integer(id_solution),
    is.list(metadata)
  )
  ## further checks
  assertthat::assert_that(
    terra::hasValues(solution_raster),
    terra::nlyr(solution_raster) == 1,
    terra::compareGeom(
      solution_raster, restopt_problem$data$existing_habitat, stopOnError = FALSE
    )
  )
  # convert object to RestoptSolution
  levels(solution_raster) <- data.frame(
    id = c(0, 1, 2, 3),
    label = c("Locked out", "Available", "Habitat", "Restoration")
  )
  names(solution_raster) <- paste0(
    get_settings(restopt_problem)$solution_name_prefix,
    id_solution
  )
  solution <- as(solution_raster, "RestoptSolution")
  solution@metadata <- metadata
  solution@problem <- restopt_problem
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
#' @param restopt_solution [`restopt_solution()`] Restopt solution
#' to this solution.
#'
#' @param area_unit `unit` object or a `character` that can be coerced to an area
#' unit (see `unit` package), or "cells" for number of cells from the original
#' habitat raster). Unit for areas. If the input habitat raster does not use a projected
#' coordinate system, only "cells" is available. Default is "ha"
#'
#' @param distance_unit `unit` object or a `character` that can be coerced to an area
#' unit (see `unit` package), or "cells" for number of cell width from the original
#' habitat raster). Unit for distances. If the input habitat raster does not use a projected
#' coordinate system, only "cells" is available. Default is "m"
#'
#' @return A `list` containing the characteristics of the restopt solution.
#'
#' @export
get_metadata <- function(restopt_solution, area_unit = "ha", distance_unit = "m") {
  assertthat::assert_that(
    inherits(restopt_solution, "RestoptSolution"),
    (area_unit == "cells" || units::ud_are_convertible(area_unit, "ha")),
    (distance_unit == "cells" || units::ud_are_convertible(distance_unit, "m"))
  )
  metadata <- restopt_solution@metadata
  if (area_unit != "cells") {
    min_rest <- as.numeric(metadata$min_restore)
    max_rest <- as.numeric(metadata$total_restorable)
    original_habitat <- get_original_habitat(restopt_solution@problem)
    existing_habitat <- get_existing_habitat(restopt_solution@problem)
    metadata$min_restore <- nb_cell_to_area(original_habitat, min_rest, area_unit)
    metadata$total_restorable <- nb_cell_to_area(original_habitat, max_rest, area_unit)
    if ("mesh_initial" %in% names(metadata)) {
      metadata$mesh_initial <- nb_cell_to_area(existing_habitat, metadata$mesh_initial, area_unit)
      metadata$mesh_best <- nb_cell_to_area(existing_habitat, metadata$mesh_best, area_unit)
    }
    if ("mesh" %in% names(metadata)) {
      metadata$mesh <- nb_cell_to_area(existing_habitat, metadata$mesh, area_unit)
    }
  }
  if (distance_unit != "cells") {
    existing_habitat <- get_existing_habitat(restopt_solution@problem)
    width <- cell_width(existing_habitat, unit = distance_unit)
    diameter <- metadata$diameter * width
    metadata$diameter <- diameter
  }
  return(metadata)
}
