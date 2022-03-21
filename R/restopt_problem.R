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
#' that describe the spatial distribution of existing habitat (potentially at
#' high resolution), and parameters to derive a downsampled existing habitat
#' raster, suitable for a tractable optimization, and a restorable habitat
#' raster. Constraints can be added to a restopt problem using
#' `add_****_constraint()` functions, and an optimization objective can be set
#' using `set_****_objective()` functions.
#'
#' @param existing_habitat [terra::rast()] Raster object containing binary
#' values that indicate if each planning unit contains habitat or not. Cells
#' with the value `1` must correspond to existing habitat. Cells with the value
#' `0` must correspond to degraded (or simply non-habitat) areas. Finally,
#' `NA` (or `NO_DATA`) cells are considered to be outside of the landscape.
#' This raster can have a high resolution, the `aggregation_factor` and the
#' `habitat_threshold` parameters, described below, will be used to down sample
#' the habitat raster to a tractable resolution for the optimization engine, and
#' automatically derive the restorable habitat raster.
#'
#' @param habitat_threshold Numeric between 0 and 1, which corresponds to the
#' minimum proportion of habitat that must be present within an aggregated pixel
#' to consider it as an habitat pixel.
#'
#' @param aggregation_factor Integer greater than 1, which corresponds to the
#' aggregation factor for down sampling the data. For example, if
#' `aggregation_factor = 2`, aggregated pixel will contain 4 original pixel.
#' See `terra::aggregate()` for more details.
#'
#' @return A new restoration problem (`RestoptProblem`) object.
#'
#' @details This function creates the base restoration optimization problem
#' object, that can be further extended with constraints and optimization
#' objectives. One input rasters is necessary to instantiate a restopt problem:
#' the `existing_habitat` raster (potentially with high resolution). This raster
#' must contains data about where are habitat areas (raster value `1`),
#' non-habitat areas (raster value `0`), and areas that must not be considered
#' during the solving procedure (`NA` or `NO_DATA`). The `aggregation_factor`
#' parameter is used to down sample the `existing_habitat` to a resolution that
#' will be tractable for the optimization engine, and the `habitat_threshold`
#' parameter indicates the minimum proportion of habitat required in aggregated
#' habitat pixels to consider them as habitat.
#'
#' @examples
#' \dontrun{
#' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#'
#' # create problem
#' p <- restopt_problem_2(
#'        existing_habitat = habitat_data,
#'        aggregation_factor = 4,
#'        habitat_threshold = 0.7
#' )
#'
#' # Plot down sampled data
#' plot(c(p$data$existing_habitat, p$data$restorable_habitat))
#'
#' # print problem
#' print(p)
#' }
#'
#' @export
restopt_problem <- function(existing_habitat, habitat_threshold = 1, aggregation_factor = 1) {
  # assert arguments are valid
  ## initial checks
  assertthat::assert_that(
    inherits(existing_habitat, "SpatRaster")
  )
  preprocessed <- prepare_inputs(
     habitat = existing_habitat,
     habitat_threshold = habitat_threshold,
     aggregation_factor = aggregation_factor
  )
  habitat_down <- preprocessed[[1]]
  restorable_down <- preprocessed[[2]]
  cell_area <- preprocessed[[3]]
  # return object
  set_no_objective(
    structure(
      list(
        data = list(
          habitat_original = existing_habitat,
          existing_habitat = habitat_down,
          restorable_habitat = restorable_down,
          aggregation_factor = aggregation_factor,
          cell_area = cell_area,
          habitat_threshold = habitat_threshold,
          locked_out = round(restorable_down <= 0)
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
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#'
#' # create problem
#' p <- restopt_problem_2(
#'        existing_habitat = habitat_data,
#'        aggregation_factor = 4,
#'        habitat_threshold = 0.7
#' )
#'
#' # print problem
#' print(p)
#' }
#'
#' @export
print.RestoptProblem <- function(x, ...) {
  message(crayon::bold(crayon::green(paste0(
      "-----------------------------------------------------------------",
    "\n                         Restopt problem                         "),
    "\n-----------------------------------------------------------------")))
  source_habitat <- basename(terra::sources(x$data$existing_habitat)[[1]])
  source_restorable <- basename(terra::sources(x$data$restorable_habitat)[[1]])
  message(
    crayon::bold(crayon::silver("existing habitat:    ")),
    crayon::cyan(ifelse(source_habitat != "", source_habitat, "in memory"))
  )
  message(
    crayon::bold(crayon::silver("restorable habitat:  ")),
    crayon::cyan(ifelse(source_restorable != "", source_restorable, "in memory"))
  )
  message(
    crayon::green("-----------------------------------------------------------------")
  )
  message(
    crayon::bold(crayon::silver("objective:           ")),
    crayon::blue(ifelse(is.null(x$objective), "none defined", x$objective$name))
  )
  message(
    crayon::green("-----------------------------------------------------------------")
  )
  message(
    crayon::bold(crayon::silver("constraints:        ")),
    ifelse(length(x$constraints) == 0, "none defined", "")
  )
  for (i in seq_along(x$constraints)) {
    message(crayon::blue(
      "  - ",
      x$constraints[[i]]$name
    ))
  }
  message(
    crayon::green("-----------------------------------------------------------------")
  )
  message(
    crayon::bold(crayon::silver("settings:\n")),
    paste(crayon::magenta(
      paste0("  - ", names(x$settings), " = ", unlist(x$settings, use.names = FALSE)),
      collapse = "\n"
    ))
  )
  message(
    crayon::bold(crayon::green("-----------------------------------------------------------------\n"))
  )
}

#' Generic function to add a constraint to a restoration optimization problem
#' For internal use only.
#'
#' @inheritParams set_max_mesh_objective
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

#' Generic function to set an objective to a restoration optimization problem
#' For internal use only.
#'
#' @inheritParams set_max_mesh_objective
#'
#' @param objective `RestoptObjective` Objective object.
#'
#' @noRd
set_restopt_objective <- function(problem, objective) {
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

  # set objective
  problem$objective <- objective

  # return updated problem
  problem
}
