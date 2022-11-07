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
#' @param habitat_threshold `numeric` number between 0 and 1, which corresponds to the
#' minimum proportion of habitat that must be present within an aggregated pixel
#' to consider it as an habitat pixel.
#'
#' @param aggregation_factor `integer` Integer greater than 1, which corresponds to the
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
#' habitat pixels to consider them as habitat. Note that An aggregated pixel
#' will contain at most `aggregation_factor^2` pixels from the input `habitat`
#' raster (`cell_area` raster in this function outputs). If an aggregated pixel
#' is close to the spatial boundaries of the problem (i.e. NA cells), it can
#' contain less than `aggregation_factor^2` fine grained pixel. You can get
#' the results of this preprocessing phase using the following methods:
#' `get_original_habitat()` (original habitat), `get_existing_habitat()`
#' (aggregated habitat), `get_cell_area()` (number of pixels in each aggregated
#' cells), and `get_restorable_area()` (amount of restorable area -- in number
#' of original raster pixels).
#'
#'@return None.
#'
#' @examples
#' \donttest{
#' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#'
#' # create problem
#' p <- restopt_problem(
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
  if (aggregation_factor == 1 && habitat_threshold < 1) {
    warning(paste("The habitat threshold parameter was automatically set to 1,",
                  "as the aggregation factor is 1"))
    habitat_threshold <- 1
  }
  preprocessed <- preprocess_input(
     habitat = existing_habitat,
     habitat_threshold = habitat_threshold,
     aggregation_factor = aggregation_factor
  )
  habitat_down <- preprocessed$existing_habitat
  restorable_down <- preprocessed$restorable_habitat
  cell_area <- preprocessed$cell_area
  levels(habitat_down) <- data.frame(
    id = c(0, 1),
    label = c(
        paste("< ", habitat_threshold * 100,  "% habitat"),
        paste("\u2265 ", habitat_threshold * 100,  "% habitat")
    )
  )
  names(habitat_down) <- "Existing habitat (aggregated)"
  names(restorable_down) <- "Restorable habitat (aggregated)"
  names(cell_area) <- "Cell area (aggregated)"
  # return object
  set_no_objective(
    structure(
      list(
        data = list(
          original_habitat = existing_habitat,
          existing_habitat = habitat_down,
          restorable_habitat = restorable_down,
          aggregation_factor = aggregation_factor,
          cell_area = cell_area,
          habitat_threshold = habitat_threshold,
          locked_out = round(restorable_down <= 0)
        ),
        constraints = list(),
        objective = NULL,
        settings = list(
          precision = 4L,
          time_limit = 0L,
          nb_solutions = 1L,
          optimality_gap = 0,
          solution_name_prefix = "Solution "
        )
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
#' \donttest{
#' #' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#'
#' # create problem
#' p <- restopt_problem(
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
  # print header
  cat(crayon::bold(crayon::green(paste(rep("-", 65), collapse = ""))), "\n")
  cat(crayon::bold(crayon::green(
    paste0(
      paste(rep(" ", 25), collapse = ""),
      "Restopt",
      paste(rep(" ", 25), collapse = "")
    )
  )), "\n")
  cat(crayon::bold(crayon::green(paste(rep("-", 65), collapse = ""))), "\n")

  # print data and parameters
  source_original_habitat <-
    basename(terra::sources(x$data$original_habitat)[[1]])
  source_habitat <- basename(terra::sources(x$data$existing_habitat)[[1]])
  source_restorable <- basename(terra::sources(x$data$restorable_habitat)[[1]])
  cat(
    crayon::bold(crayon::white("original habitat:    ")),
    crayon::cyan(
      ifelse(
        source_original_habitat != "",
        source_original_habitat, "in memory"
      )
    ),
    "\n"
  )
  cat(
    crayon::bold(crayon::white("aggregation factor:  ")),
    crayon::cyan(x$data$aggregation_factor),
    "\n"
  )
  cat(
    crayon::bold(crayon::white("habitat threshold:   ")),
    crayon::cyan(x$data$habitat_threshold),
    "\n"
  )
  cat(
    crayon::bold(crayon::white("existing habitat:    ")),
    crayon::cyan(
      ifelse(source_habitat != "", source_habitat, "in memory")
    ),
    "\n"
  )
  cat(
    crayon::bold(crayon::white("restorable habitat:  ")),
    crayon::cyan(
      ifelse(source_restorable != "", source_restorable, "in memory")
    ),
    "\n"
  )
  cat(crayon::green(paste(rep("-", 65), collapse = "")), "\n")

  # print objective
  cat(
    crayon::bold(crayon::white("objective:           ")),
    crayon::blue(
      ifelse(is.null(x$objective), "none defined", x$objective$name)
    ),
    "\n"
  )
  cat(crayon::green(paste(rep("-", 65), collapse = "")), "\n")

  # print constraints
  cat(
    crayon::bold(crayon::white("constraints:        ")),
    ifelse(length(x$constraints) == 0, "none defined", ""),
    "\n"
  )
  for (i in seq_along(x$constraints)) {
    cat(
      crayon::blue("  - ", x$constraints[[i]]$name),
      "\n"
    )
  }
  cat(crayon::green(paste(rep("-", 65), collapse = "")), "\n")

  # print settings
  cat(crayon::bold(crayon::white("settings:")), "\n")
  cat(
    paste(
      crayon::magenta(
        paste0(
          "  - ", names(x$settings), " = ",
          unlist(x$settings, use.names = FALSE)
        ),
        collapse = crayon::reset("\n")
      )
    ),
    "\n"
  )

  # print footer
  cat(crayon::bold(crayon::green(paste(rep("-", 65), collapse = ""))), "\n")
}

#' Generic function to add a constraint to a restoration optimization problem
#' For internal use only.
#'
#' @inheritParams set_max_mesh_objective
#' @inherit set_max_mesh_objective return
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
#' @inherit set_max_mesh_objective return
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

#' Retrieve the original (i.e. not aggregated) habitat data.
#'
#' @param problem [restopt_problem()] Restoration problem object.
#'
#' @return [terra::rast()] The original (i.e. not aggregated) habitat data.
#'
#' @examples
#' \donttest{
#' #' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#'
#' # create problem
#' problem <- restopt_problem(
#'        existing_habitat = habitat_data,
#'        aggregation_factor = 4,
#'        habitat_threshold = 0.7
#' )
#'
#' plot(get_original_habitat(problem))
#' }
#'
#' @export
get_original_habitat <- function(problem) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem")
  )
  return(problem$data$original_habitat)
}

#' Retrieve the existing (i.e. aggregated) habitat data.
#'
#' @param problem [restopt_problem()] Restoration problem object.
#'
#' @return [terra::rast()] The existing (aggregated) habitat data.
#'
#' @examples
#' \donttest{
#' #' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#'
#' # create problem
#' problem <- restopt_problem(
#'        existing_habitat = habitat_data,
#'        aggregation_factor = 4,
#'        habitat_threshold = 0.7
#' )
#'
#' plot(get_existing_habitat(problem))
#' }
#'
#' @export
get_existing_habitat <- function(problem) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem")
  )
  return(problem$data$existing_habitat)
}

#' Retrieve the restorable habitat (aggregated) data.
#'
#' @param problem [restopt_problem()] Restoration problem object.
#'
#' @return [terra::rast()] The restorable habitat (aggregated) data.
#'
#' @examples
#' \donttest{
#' #' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#'
#' # create problem
#' problem <- restopt_problem(
#'        existing_habitat = habitat_data,
#'        aggregation_factor = 4,
#'        habitat_threshold = 0.7
#' )
#'
#' plot(get_restorable_habitat(problem))
#' }
#'
#' @export
get_restorable_habitat <- function(problem) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem")
  )
  return(problem$data$restorable_habitat)
}

#' Retrieve the aggregation factor of a restopt problem.
#'
#' @param problem [restopt_problem()] Restoration problem object.
#'
#' @return `numeric` The aggregation factor of the restopt problem.
#'
#' @examples
#' \donttest{
#' #' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#'
#' # create problem
#' problem <- restopt_problem(
#'        existing_habitat = habitat_data,
#'        aggregation_factor = 4,
#'        habitat_threshold = 0.7
#' )
#'
#' get_aggregation_factor(problem)
#' }
#'
#' @export
get_aggregation_factor <- function(problem) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem")
  )
  return(problem$data$aggregation_factor)
}

#' Retrieve the habitat threshold parameter of a restopt problem.
#'
#' @param problem [restopt_problem()] Restoration problem object.
#'
#' @return `numeric` The habitat threshold parameter of the restopt problem.
#'
#' @examples
#' \donttest{
#' #' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#'
#' # create problem
#' problem <- restopt_problem(
#'        existing_habitat = habitat_data,
#'        aggregation_factor = 4,
#'        habitat_threshold = 0.7
#' )
#'
#' get_habitat_threshold(problem)
#' }
#'
#' @export
get_habitat_threshold <- function(problem) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem")
  )
  return(problem$data$habitat_threshold)
}

#' Retrieve the locked out areas of a restopt problem.
#'
#' @param problem [restopt_problem()] Restoration problem object.
#'
#' @return [terra::rast()] The locked out areas of the restopt problem.
#'
#' @examples
#' \donttest{
#' #' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#'
#' # create problem
#' problem <- restopt_problem(
#'        existing_habitat = habitat_data,
#'        aggregation_factor = 4,
#'        habitat_threshold = 0.7
#' )
#'
#' get_locked_out_areas(problem)
#' }
#'
#' @export
get_locked_out_areas <- function(problem) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem")
  )
  return(problem$data$locked_out)
}

#' Retrieve the aggregated cell area of a restopt problem.
#'
#' @param problem [restopt_problem()] Restoration problem object.
#'
#' @return [terra::rast()] The aggregated cell area of the restopt problem.
#'
#' @examples
#' \donttest{
#' #' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#'
#' # create problem
#' problem <- restopt_problem(
#'        existing_habitat = habitat_data,
#'        aggregation_factor = 4,
#'        habitat_threshold = 0.7
#' )
#'
#' get_cell_area(problem)
#' }
#'
#' @export
get_cell_area <- function(problem) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem")
  )
  return(problem$data$cell_area)
}

#' Retrieve the constraints of a restopt problem.
#'
#' @param problem [restopt_problem()] Restoration problem object.
#'
#' @return `list` The constraints of the restopt problem.
#'
#' @examples
#' \donttest{
#' #' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#'
#' # create problem
#' problem <- restopt_problem(
#'        existing_habitat = habitat_data,
#'        aggregation_factor = 4,
#'        habitat_threshold = 0.7
#' )
#'
#' get_constraints(problem)
#' }
#'
#' @export
get_constraints <- function(problem) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem")
  )
  return(problem$constraints)
}

#' Retrieve the optimization objective of a restopt problem.
#'
#' @param problem [restopt_problem()] Restoration problem object.
#'
#' @return `RestoptObjectve` The optimization objective of the restopt problem.
#'
#' @examples
#' \donttest{
#' #' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#'
#' # create problem
#' problem <- restopt_problem(
#'        existing_habitat = habitat_data,
#'        aggregation_factor = 4,
#'        habitat_threshold = 0.7
#' )
#'
#' get_objective(problem)
#' }
#'
#' @export
get_objective <- function(problem) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem")
  )
  return(problem$objective)
}

#' Retrieve the settings of a restopt problem.
#'
#' @param problem [restopt_problem()] Restoration problem object.
#'
#' @return `list` The settings associated with the restopt problem.
#'
#' @examples
#' \donttest{
#' #' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#'
#' # create problem
#' problem <- restopt_problem(
#'        existing_habitat = habitat_data,
#'        aggregation_factor = 4,
#'        habitat_threshold = 0.7
#' )
#'
#' get_settings(problem)
#' }
#'
#' @export
get_settings <- function(problem) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem")
  )
  return(problem$settings)
}
