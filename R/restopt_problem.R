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
#' Create a new restoration optimization problem using data that describe
#' the spatial distribution of existing habitat and potential for
#' restoration.
#'
#' @param existing_habitat [terra::rast()] Raster object containing binary
#' values that indicate if each planning unit contains habitat or not.
#'
#' @param restorable_habitat [terra::rast()] Raster object containing
#' integer values that indicate the amount of habitat that can
#' be restored within each planning unit.
#'
#' @return A new restoration problem (`RestoptProblem`) object.
#'
#' @details
#' TODO.
#'
#' @examples
#' # TODO
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
    terra::global(restorable_habitat, "min", na.rm = TRUE)[[]] >= 0,
    msg = paste(
      "argument to \"restorable_habitat\" must have integer values greater",
      "than 0"
    )
  )

  # return object
  structure(
    list(
      data = list(
        existing_habitat = existing_habitat,
        restorable_habitat = restorable_habitat,
        locked_out = list(
          data = round(restorable_habitat > 0),
          raster_value = 0,
          lock_out = TRUE
        )
      ),
      constraints = list(),
      objective = NULL,
      settings = list(precision = 4L, time_limit = 0L)
    ),
    class = "RestoptProblem"
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
#' TODO.
#'
#' @export
print.RestoptProblem <- function(x, ...) {
  message("Restopt problem")
  message(
    "existing habitat:   ",
    basename(terra::sources(x$data$existing_habitat)$source[[1]])
  )
  message(
    "restorable habitat: ",
    basename(terra::sources(x$data$restorable_habitat)$source[[1]])
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

#' Add a constraint to a restoration optimization problem
#'
#' @inheritParams add_max_mesh_objective
#'
#' @param constraint `RestoptConstraint` Constraint object.
#'
#' @examples
#' TODO.
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

#' Add an objective to a restoration optimization problem
#'
#' Display information about a restoration
#'
#' @inheritParams add_max_mesh_objective
#'
#' @param objective `RestoptObjective` Objective object.
#'
#' @examples
#' TODO.
#'
#' @noRd
add_restopt_objective <- function(problem, objective) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem"),
    inherits(objective, "RestoptObjectve")
  )

  # throw warning if objective specified
  if (!is.null(problem$objective)) {
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
