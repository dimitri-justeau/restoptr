#' @include internal.R
NULL

#' Add constraint to specify the available amount of surface for restoration
#'
#' Add constraint to a restoration problem ([restopt_problem()]) object
#' to specify specify the available amount of surface for restoration
#'
#' @inheritParams set_max_mesh_objective
#'
#' @param min_restore `integer` Minimum allowed area to restore in the solution.
#'
#' @param max_restore `integer` Maximum allowed area to restore in the solution
#'
#' @param cell_area `integer` Total area of a cell.
#'
#' @param min_proportion `float` Minimum habitat proportion to consider a cell
#' as restored.
#'
#' @details Given the `restorable_habitat` input raster in \link{restopt_problem},
#' this constraint ensures that the total amount of restorable habitat in
#' selected planning units is at least `min_restore` and at most `max_restore`.
#' The `min_proportion` parameter is a numeric between 0 and 1, and correspond to the
#' minimum proportion of habitat area that needs to be restored in the planning
#' unit to consider the planning unit as restored. This proportion is relative
#' to the area of a planning unit, the `cell_area` parameter, which must be
#' consistent with the values provided in the `restorable_habitat` input
#' raster. The same applies for the `min_restore` and `max_restore` parameters.
#' Note that when a solution is found, the "maximum restorable habitat" is
#' displayed, this value does not correspond to the `max_restore` parameter,
#' but to the total area that can be restored in the selected planning units.
#' The `max_restore` parameter is actually an upper bound of the minimum habitat
#' that needs to be restored to reach the `min_proportion` of habitat in every
#' selected planning units.
#'
#' @examples
#' \dontrun{
#' #load data
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
#' ) %>%
#' add_restorable_constraint(
#'   min_restore = 200,
#'   max_restore = 300,
#'   cell_area = 23,
#'   min_proportion = 0.7
#' ) %>%
#' add_compactness_constraint(5)
#'
#' # print problem
#' print(p)
#'
#' # Solve problem
#' s <- solve(p)
#' # plot solution
#' plot(
#'   x = s, main = "solution",
#'   col = c("#E5E5E5", "#fff1d6", "#b2df8a", "#1f78b4")
#' )
#' }
#'
#' @export
add_restorable_constraint <- function(problem,
                                      min_restore,
                                      max_restore,
                                      cell_area,
                                      min_proportion = 1) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem"),
    assertthat::is.count(min_restore),
    assertthat::noNA(min_restore),
    assertthat::is.count(max_restore),
    assertthat::noNA(max_restore),
    assertthat::is.count(cell_area),
    assertthat::noNA(cell_area),
    assertthat::is.number(min_proportion),
    assertthat::noNA(min_proportion),
    min_proportion >= 0,
    min_proportion <= 1,
    max_restore >= min_restore,
    cell_area > 0
  )

  # add constraint
  add_restopt_constraint(
    problem = problem,
    constraint = restopt_component(
      name = paste0(
        "restorable (",
        "min_restore = ", as.integer(min_restore),
        ", max_restore = ", max_restore, ")"
      ),
      class = c("RestorableConstraint", "RestoptConstraint"),
      post = function(jproblem) {
        rJava::.jcall(
          jproblem, "V", "postRestorableConstraint",
          as.integer(min_restore), as.integer(max_restore),
          as.integer(cell_area), min_proportion
        )
      }
    )
  )
}

#' @export
add_restorable_constraint_2 <- function(problem,
                                      min_restore,
                                      max_restore,
                                      min_proportion = 1) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem"),
    assertthat::is.count(min_restore),
    assertthat::noNA(min_restore),
    assertthat::is.count(max_restore),
    assertthat::noNA(max_restore),
    assertthat::is.number(min_proportion),
    assertthat::noNA(min_proportion),
    min_proportion >= 0,
    min_proportion <= 1,
    max_restore >= min_restore
  )

  # add constraint
  add_restopt_constraint(
    problem = problem,
    constraint = restopt_component(
      name = paste0(
        "restorable (",
        "min_restore = ", as.integer(min_restore),
        ", max_restore = ", max_restore, ")"
      ),
      class = c("RestorableConstraint", "RestoptConstraint"),
      post = function(jproblem) {
        cell_area <- problem$data$cell_area
        rJava::.jcall(
          jproblem, "V", "postRestorableConstraint",
          as.integer(min_restore), as.integer(max_restore),
          .jarray(as.integer(as.vector(problem$data$cell_area))), min_proportion
        )
      }
    )
  )
}
