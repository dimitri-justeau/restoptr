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

#' S4 class wrapper around a restopt base problem instance
#'
#' @description Generate landscape raster from landscape structure
#'
#' @import rJava
#'
#'
setClass(
  "RestoptProblem",
  slots = c(
    habitat_path = "character",
    accessible_path = "character",
    restorable_path = "character",
    accessible_value = "integer",
    min_nb_components = "integer",
    max_nb_components = "integer",
    max_diameter = "numeric",
    min_restore = "integer",
    max_restore = "integer",
    cell_area = "integer",
    min_proportion = "numeric"
  )
)

#' Restopt base problem constructor
#'
#' @param habitat_path Path to the habitat input raster file (either habitat_path or habitat must be given)
#' @param habitat Habitat input raster object (either habitat_path or habitat must be given)
#' @param restorable_path Path to the restorable input raster file
#' @param restorable Restorable input raster object (either restorable_path or restorable must be given)
#' @param accessible_path Path to the accessible input raster file
#' @param accessible Accessible input raster object (either accessible_path or accessible must be given)
#' @param accessible_value Value of accessible areas in the accessible input raster
#'
#' @export
#'
RestoptProblem <- function(habitat, habitat_path, restorable, restorable_path, accessible, accessible_path, accessible_value=1) {
  # Check arguments
  #  Habitat
  if (missing(habitat_path)) {
    if (missing(habitat)) {
      stop("Either habitat or habitat_path must be used to construct a restopt base problem")
    }
    if (terra::inMemory(habitat)) {
      habitat_path <- terra::sources(terra::writeRaster(habitat, tempfile(fileext = ".tif")))[[1]]
    } else {
      habitat_path <- terra::sources(habitat)[[1]]
    }
  } else {
    if (!missing(habitat)) {
      stop("Either habitat or habitat_path must be used to construct a restopt base problem")
    }
  }
  #  Restorable
  if (missing(restorable_path)) {
    if (missing(restorable)) {
      stop("Either restorable or restorable_path must be used to construct a restopt base problem")
    }
    if (terra::inMemory(restorable)) {
      restorable_path <- terra::sources(terra::writeRaster(restorable, tempfile(fileext = ".tif")))[[1]]
    } else {
      restorable_path <- terra::sources(restorable)[[1]]
    }
  } else {
    if (!missing(restorable)) {
      stop("Either restorable or restorable_path must be used to construct a restopt base problem")
    }
  }
  #  Accessible
  if (missing(accessible_path)) {
    if (missing(accessible)) {
      stop("Either accessible or accessible_path must be used to construct a restopt base problem")
    }
    if (terra::inMemory(accessible)) {
      accessible_path <- terra::sources(terra::writeRaster(accessible, tempfile(fileext = ".tif")))[[1]]
    } else {
      accessible_path <- terra::sources(accessible)[[1]]
    }
  } else {
    if (!missing(accessible)) {
      stop("Either accessible or accessible_path must be used to construct a restopt base problem")
    }
  }
  # Construct the base problem
  new(
    "RestoptProblem",
    habitat_path = habitat_path,
    restorable_path = restorable_path,
    accessible_path = accessible_path,
    accessible_value = as.integer(accessible_value),
    min_nb_components = NA_integer_,
    max_nb_components = NA_integer_,
    max_diameter = NA_real_,
    min_restore = NA_integer_,
    max_restore = NA_integer_,
    cell_area = NA_integer_,
    min_proportion = NA_real_
  )
}

#' Post a number of connected components constraint to the problem
#'
#' @param problem A RestoptProblem instance
#' @param min_nb_components minimum number of connected components allowed in the solution
#' @param max_nb_components maximum number of connected components allowed in the solution
#'
#' @return the updated problem
#'
#' @export
#'
setGeneric("postNbComponentsConstraint", function(problem, min_nb_components, max_nb_components) standardGeneric("postNbComponentsConstraint"))
setMethod(
  "postNbComponentsConstraint",
  "RestoptProblem",
  function(problem, min_nb_components, max_nb_components) {
    problem@min_nb_components <- as.integer(min_nb_components)
    problem@max_nb_components <- as.integer(max_nb_components)
    return(problem)
  }
)

#' Post a maximum diameter constraint to the problem
#'
#' @param problem A RestoptProblem instance
#' @param max_diameter maxmimum allowed diameter for the smallest enclosing circle of the solution
#'
#' @return the updated problem
#'
#' @export
#'
setGeneric("postCompactnessConstraint", function(problem, max_diameter) standardGeneric("postCompactnessConstraint"))
setMethod(
  "postCompactnessConstraint",
  "RestoptProblem",
  function(problem, max_diameter) {
    problem@max_diameter <- max_diameter
    return(problem)
  }
)

#' Post a restorable area constraint to the problem
#'
#' @param problem A RestoptProblem instance
#' @param min_restore minimum allowed area to restore in the solution
#' @param max_restore maximum allowed area to restore in the solution
#' @param cell_area total cell area
#' @param min_proportion minimum habitat proportion to consider a cell as restored
#'
#' @return the updated problem
#'
#' @export
#'
setGeneric("postRestorableConstraint", function(problem, min_restore, max_restore, cell_area, min_proportion) standardGeneric("postRestorableConstraint"))
setMethod(
  "postRestorableConstraint",
  "RestoptProblem",
  function(problem, min_restore, max_restore, cell_area, min_proportion) {
    problem@min_restore <- as.integer(min_restore)
    problem@max_restore <- as.integer(max_restore)
    problem@cell_area <- as.integer(cell_area)
    problem@min_proportion <- min_proportion
    return(problem)
  }
)

#' Return TRUE if a number of components constraints was posted to the model
#'
#' @return TRUE if a number of components constraints was posted to the model, otherwise FALSE
#'
#' @export
#'
setGeneric("hasNbComponentsConstraint", function(problem) standardGeneric("hasNbComponentsConstraint"))
setMethod(
  "hasNbComponentsConstraint",
  "RestoptProblem",
  function(problem) {
    return(!is.na(problem@min_nb_components) && !is.na(problem@max_nb_components))
  }
)

#' Return TRUE if a compactness constraint was posted to the model
#'
#' @return TRUE if a compactness constraint was posted to the model, otherwise FALSE
#'
#' @export
#'
setGeneric("hasCompactnessConstraint", function(problem) standardGeneric("hasCompactnessConstraint"))
setMethod(
  "hasCompactnessConstraint",
  "RestoptProblem",
  function(problem) {
    return(!is.na(problem@max_diameter))
  }
)

#' Return TRUE if a restorable area constraint was posted to the model
#'
#' @return TRUE if a restorable area constraint was posted to the model, otherwise FALSE
#'
#' @export
#'
setGeneric("hasRestorableConstraint", function(problem) standardGeneric("hasRestorableConstraint"))
setMethod(
  "hasRestorableConstraint",
  "RestoptProblem",
  function(problem) {
    return(!is.na(problem@min_restore) && !is.na(problem@max_restore && !is.na(problem@cell_area) && !is.na(problem@min_proportion)))
  }
)


#' Find a solution maximizing the effective mesh size (MESH)
#'
#' @param problem A RestoptProblem instance
#' @param precision precision for MESH calculation
#' @param time_limit time limit for solving (use 0 for no time limit)
#'
#' @export
#'
setGeneric("maximizeMESH", function(problem, precision, time_limit=0) standardGeneric("maximizeMESH"))
setMethod(
  "maximizeMESH",
  "RestoptProblem",
  function(problem, precision, time_limit=0) {
    jData <- .jnew("org.restopt.DataLoader", problem@habitat_path, problem@accessible_path, problem@restorable_path)
    jProblem <- .jnew("org.restopt.BaseProblem", jData, problem@accessible_value)
    if (hasNbComponentsConstraint(problem)) {
      .jcall(jProblem, "V", "postNbComponentsConstraint", problem@min_nb_components, problem@max_nb_components)
    }
    if (hasCompactnessConstraint(problem)) {
      .jcall(jProblem, "V", "postCompactnessConstraint", problem@max_diameter)
    }
    if (hasRestorableConstraint(problem)) {
      .jcall(jProblem, "V", "postRestorableConstraint", problem@min_restore, problem@max_restore, problem@cell_area, problem@min_proportion)
    }
    output <- tempfile(fileext = "")
    if (.jcall(jProblem, "Z", "maximizeMESH", as.integer(precision), output, as.integer(time_limit))) {
      r <- terra::rast(paste(output, ".tif", sep = ""))
      attributes(r)$metadata <- read.csv(paste(output, ".csv", sep = ""))
      return(r)
    }
  }
)

#' Find a solution maximizing the integral index of connectivity (IIC)
#'
#' @param problem A RestoptProblem instance
#' @param precision precision for IIC calculation
#' @param time_limit time limit for solving (use 0 for no time limit)
#'
#' @export
#'
setGeneric("maximizeIIC", function(problem, precision, time_limit=0) standardGeneric("maximizeIIC"))
setMethod(
  "maximizeIIC",
  "RestoptProblem",
  function(problem, precision, time_limit=0) {
    jData <- .jnew("org.restopt.DataLoader", problem@habitat_path, problem@accessible_path, problem@restorable_path)
    jProblem <- .jnew("org.restopt.BaseProblem", jData, problem@accessible_value)
    if (hasNbComponentsConstraint(problem)) {
      .jcall(jProblem, "V", "postNbComponentsConstraint", problem@min_nb_components, problem@max_nb_components)
    }
    if (hasCompactnessConstraint(problem)) {
      .jcall(jProblem, "V", "postCompactnessConstraint", problem@max_diameter)
    }
    if (hasRestorableConstraint(problem)) {
      .jcall(jProblem, "V", "postRestorableConstraint", problem@min_restore, problem@max_restore, problem@cell_area, problem@min_proportion)
    }
    output <- tempfile(fileext = "")
    if (.jcall(jProblem, "Z", "maximizeIIC", as.integer(precision), output, as.integer(time_limit))) {
      r <- terra::rast(paste(output, ".tif", sep = ""))
      attributes(r)$metadata <- read.csv(paste(output, ".csv", sep = ""))
      return(r)
    }
  }
)

