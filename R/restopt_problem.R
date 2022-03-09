#' @include internal.R
NULL

# Copyright (c) 2021, Dimitri Justeau-Allaire
#
# Institut Agronomique neo-Caledonien (IAC), 98800 Noumea, New Caledonia
# AMAP, Univ Montpellier, CIRAD, CNRS, INRA, IRD, Montpellier, France
#
# This file is part of restoptr
#
# restoptr is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# restoptr is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with restoptr  If not, see <https://www.gnu.org/licenses/>.

#' Restoration optimization problem
#'
#' Create a new restoration optimization problem (`RestoptProblem`) using data
#' that describe the spatial distribution of existing habitat and potential for
#' restoration. Constraints can be added to a restopt problem using
#' `add_****_constraint()` functions, and an optimization objective can be set
#' using `add_****_objective()` functions.
#'
#' @param existing_habitat [terra::rast()] Raster object containing binary
#' values that indicate if each planning unit contains habitat or not. Cells
#' with the value `1` must correspond to existing habitat. Cells with the value
#' `0` must correspond to degraded (or simply non-habitat) areas. Finally,
#' `NA` (or `NO_DATA`) cells are considered to be outside of the landscape.
#'
#' @param restorable_habitat [terra::rast()] Raster object containing
#' integer values that indicate the amount of habitat that can
#' be restored within each planning unit. Note that if the raster contains
#' real values, they will be rounded to integer during the solving procedure.
#'
#' @return A new restoration problem (`RestoptProblem`) object.
#'
#' @details This function creates the base restoration optimization problem
#' object, that can be further extended with constraints and optimization
#' objectives. Two input rasters are necessary to instantiate a restopt problem:
#' the `existing_habitat` raster and the `restorable_habitat` raster. The first
#' which contains data about where are habitat areas (raster value `1`),
#' non-habitat areas (raster value `0`), and areas that must not be considered
#' during the solving procedure (`NA` or `NO_DATA`). The second raster indicates,
#' for each non-habitat planning unit, the amount of habitat that can be restored.
#' **Important:** Both input raster must have the same dimensions and the same
#' spatial extent.
#'
#' @examples
#' \dontrun{
#' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat.tif", package = "restoptr")
#' )
#'
#' restorable_data <- rast(
#'   system.file("extdata", "restorable.tif", package = "restoptr")
#' )
#'
#' # plot data
#' plot(rast(list(habitat_data, restorable_data)), nc = 2)
#'
#' # create problem
#' p <- restopt_problem(
#'        existing_habitat = habitat_data,
#'        restorable_habitat = restorable_data
#' )
#' # print problem
#' print(p)
#' }
#'
#' @export
restopt_problem <- function(existing_habitat, restorable_habitat) {
  # assert arguments are valid
  ## initial checks
  assertthat::assert_that(
    inherits(existing_habitat, "SpatRaster"),
    inherits(restorable_habitat, "SpatRaster")
  )
  ## further checks
  assertthat::assert_that(
    terra::hasValues(existing_habitat),
    terra::hasValues(restorable_habitat),
    terra::nlyr(existing_habitat) == 1,
    terra::nlyr(restorable_habitat) == 1,
    terra::compareGeom(
      existing_habitat, restorable_habitat, stopiffalse = FALSE
    )
  )
  ## assert valid values
  assertthat::assert_that(
    is_binary_raster(existing_habitat),
    msg = "argument to \"existing_habitat\" must have binary values"
  )
  assertthat::assert_that(
    terra::global(restorable_habitat, "min", na.rm = TRUE)$min >= 0,
    msg = paste(
      "argument to \"restorable_habitat\" must have integer values greater",
      "than 0"
    )
  )

  # return object
  add_no_objective(
      structure(
      list(
        data = list(
          existing_habitat = existing_habitat,
          restorable_habitat = restorable_habitat,
          locked_out = list(
            data = round(restorable_habitat <= 0)
          )
        ),
        constraints = list(),
        objective = NULL,
        settings = list(precision = 4L, time_limit = 0L)
      ),
      class = "RestoptProblem"
    )
  )
}

#' Print a restoration optimization problem
#'
#' Display information about a restoration optimization problem.
#'
#' @param x [restopt_problem()] Restoration problem object.
#'
#' @param ... Arguments not used.
#'
#' @examples
#' \dontrun{
#' #' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat.tif", package = "restoptr")
#' )
#'
#' restorable_data <- rast(
#'   system.file("extdata", "restorable.tif", package = "restoptr")
#' )
#'
#' # plot data
#' plot(rast(list(habitat_data, restorable_data)), nc = 2)
#'
#' # create problem
#' p <- restopt_problem(
#'        existing_habitat = habitat_data,
#'        restorable_habitat = restorable_data
#' )
#' # print problem
#' print(p)
#' }
#'
#' @export
print.RestoptProblem <- function(x, ...) {
  message("Restopt problem")
  source_habitat <- basename(terra::sources(x$data$existing_habitat)[[1]])
  source_restorable <- basename(terra::sources(x$data$restorable_habitat)[[1]])
  message(
    "existing habitat:   ",
    ifelse(source_habitat != "", source_habitat, "in memory")
  )
  message(
    "restorable habitat: ",
    ifelse(source_restorable != "", source_habitat, "in memory")
  )
  message(
    "objective:          ",
    ifelse(is.null(x$objective), "none defined", x$objective$name)
  )
  message(
    "constraints:        ",
    ifelse(length(x$constraints) == 0, "none defined", "")
  )
  for (i in seq_along(x$constraints)) {
    message(
      "  - ",
      x$constraints[[i]]$name
    )
  }
  message(
    "settings:           ",
    paste(
      paste(names(x$settings), "=", unlist(x$settings, use.names = FALSE)),
      collapse = ", "
    )
  )
}

#' Generic function to add a constraint to a restoration optimization problem
#' For internal use only.
#'
#' @inheritParams add_max_mesh_objective
#'
#' @param constraint `RestoptConstraint` Constraint object.
#'
#' @noRd
add_restopt_constraint <- function(problem, constraint) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem"),
    inherits(constraint, "RestoptConstraint")
  )

  # throw warning if constraint specified
  i <- which(vapply(
    problem$constraints, inherits, logical(1), class(constraint)[[1]]
  ))
  if (length(i) > 0) {
    warning(
      "overwriting previously defined constraint.",
      call. = FALSE, immediate. = TRUE
    )
  } else {
    i <- length(problem$constraints) + 1
  }

  # add constraint
  problem$constraints[[i]] <- constraint

  # return updated problem
  problem
}

#' Generic function to add an objective to a restoration optimization problem
#' For internal use only.
#'
#' @inheritParams add_max_mesh_objective
#'
#' @param objective `RestoptObjective` Objective object.
#'
#' @noRd
add_restopt_objective <- function(problem, objective) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem"),
    inherits(objective, "RestoptObjectve")
  )

  # throw warning if objective specified
  if (!is.null(problem$objective) && !inherits(problem$objective, "NoObjective")) {
    warning(
      "overwriting previously defined objective.",
      call. = FALSE, immediate. = TRUE
    )
  }

  # add objective
  problem$objective <- objective

  # return updated problem
  problem
}
