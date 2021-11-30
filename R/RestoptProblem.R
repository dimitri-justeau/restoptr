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
    jProblem = "jobjRef"
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
    if (inMemory(habitat)) {
      habitat_path <- filename(raster::writeRaster(habitat, tempfile(fileext = ".tif")))
    } else {
      habitat_path <- filename(habitat)
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
    if (inMemory(restorable)) {
      restorable_path <- filename(raster::writeRaster(restorable, tempfile(fileext = ".tif")))
    } else {
      restorable_path <- filename(restorable)
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
    if (inMemory(accessible)) {
      accessible_path <- filename(raster::writeRaster(accessible, tempfile(fileext = ".tif")))
    } else {
      accessible_path <- filename(accessible)
    }
  } else {
    if (!missing(accessible)) {
      stop("Either accessible or accessible_path must be used to construct a restopt base problem")
    }
  }
  # Construct the base problem
  new(
    "RestoptProblem",
    jProblem = .jnew(
      "org.restopt.BaseProblem",
      .jnew("org.restopt.DataLoader", habitat_path, accessible_path, restorable_path),
      as.integer(accessible_value)
    )
  )
}

#' Post a number of connected components constraint to the problem
#'
#' @param problem A RestoptProblem instance
#' @param min_nbCC minimum number of connected components allowed in the solution
#' @param max_nbCC maximum number of connected components allowed in the solution
#'
#' @export
#'
setGeneric("postNbComponentsConstraint", function(problem, min_nbCC, max_nbCC) standardGeneric("postNbComponentsConstraint"))
setMethod(
  "postNbComponentsConstraint",
  "RestoptProblem",
  function(problem, min_nbCC, max_nbCC) {
    .jcall(problem@jProblem, "V", "postNbComponentsConstraint", as.integer(min_nbCC), as.integer(max_nbCC))
  }
)

#' Post a maximum diameter constraint to the problem
#'
#' @param problem A RestoptProblem instance
#' @param max_diameter maxmimum allowed diameter for the smallest enclosing circle of the solution
#'
#' @export
#'
setGeneric("postCompactnessConstraint", function(problem, max_diameter) standardGeneric("postCompactnessConstraint"))
setMethod(
  "postCompactnessConstraint",
  "RestoptProblem",
  function(problem, max_diameter) {
    .jcall(problem@jProblem, "V", "postCompactnessConstraint", max_diameter)
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
#' @export
#'
setGeneric("postRestorableConstraint", function(problem, min_restore, max_restore, cell_area, min_proportion) standardGeneric("postRestorableConstraint"))
setMethod(
  "postRestorableConstraint",
  "RestoptProblem",
  function(problem, min_restore, max_restore, cell_area, min_proportion) {
    .jcall(problem@jProblem, "V", "postRestorableConstraint", as.integer(min_restore), as.integer(max_restore), as.integer(cell_area), min_proportion)
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
    output <- tempfile(fileext = "")
    .jcall(problem@jProblem, "V", "maximizeMESH", as.integer(precision), output, as.integer(time_limit), FALSE)
    return(c(raster::raster(paste(output, ".tif", sep = "")), read.csv(paste(output, ".csv", sep = ""))))
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
    output <- tempfile(fileext = "")
    .jcall(problem@jProblem, "V", "maximizeIIC", as.integer(precision), output, as.integer(time_limit), FALSE)
    return(c(raster::raster(paste(output, ".tif", sep = "")), read.csv(paste(output, ".csv", sep = ""))))
  }
)

