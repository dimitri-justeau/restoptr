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
    terra::compareGeom(
      existing_habitat, restorable_habitat, stopiffalse = FALSE
    )
  )

  # return object
  structure(
    list(
      data = list(
        existing_habitat = existing_habitat,
        restorable_habitat = restorable_habitat
      ),
      constraints = list(),
      objective = NULL,
      settings = list(precision = 1e-5, time_limit = 0)
    ),
    class = "RestoptProblem"
  )
}

#' Print a restoration optimization problem
#'
#' Display information about a restoration optimization problem.
#'
#' @inheritParams add_max_mesh_objective
#'
#' @param ... Arguments not used.
#'
#' @examples
#' TODO.
#'
#' @export
print.RestoptProblem <- function(x, ...) {
  print(x)
}

#' Add a constraint to a restoration optimization problem
#'
#' Display information about a restoration
#'
#' @inheritParams add_max_mesh_objective
#'
#' @param y `RestoptConstraint` Constraint object.
#'
#' @examples
#' TODO.
#'
#' @noRd
add_restopt_constraint <- function(x, y) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "RestoptProblem"),
    inherits(y, "RestoptConstraint")
  )

  # throw warning if constraint specified
  i <- which(vapply(x$constraints, inherits, logical(1), class(y)[[1]]))
  if (length(i) > 0) {
    warning(
      "overwriting previously defined constraint.",
      call. = FALSE, immediate. = TRUE
    )
  } else {
    i <- length(x$constraints) + 1
  }

  # add constraint
  x$constraint[[i]] <- y

  # return updated problem
  x
}

#' Add an objective to a restoration optimization problem
#'
#' Display information about a restoration
#'
#' @inheritParams add_max_mesh_objective
#'
#' @param y `RestoptObjective` Objective object.
#'
#' @examples
#' TODO.
#'
#' @noRd
add_restopt_objective <- function(x, y) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "RestoptProblem"),
    inherits(y, "RestoptObjectve")
  )

  # throw warning if objective specified
  if (!is.null(x$objective)) {
    warning(
      "overwriting previously defined objective.",
      call. = FALSE, immediate. = TRUE
    )
  }

  # add objective
  x$objective <- y

  # return updated problem
  x
}