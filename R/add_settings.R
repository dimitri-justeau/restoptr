#' @include internal.R
NULL

#' Add settings
#'
#' Add settings to a restoration problem ([restopt_problem()]) object
#' to customize the optimization procedure.
#'
#' @inheritParams set_max_mesh_objective
#' @inherit set_max_mesh_objective return
#'
#' @param precision `integer` Precision for calculations.
#' Defaults to 4.
#'
#' @param time_limit `integer` Maximum permitted run time for optimization
#' (seconds).
#' Defaults to 0.
#'
#' @param nb_solutions `integer` Number of desired solutions. Defaults to 1.
#'
#' @param optimality_gap `numeric` Optimality gap (between 0 and 1).
#' For example, an argument of 0.1 means that solutions should be within 10%
#' of optimality.
#' Defaults to 0, such that optimal solutions are returned.
#
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
#'     existing_habitat = habitat_data,
#'     aggregation_factor = 16,
#'     habitat_threshold = 0.7
#'   ) %>%
#'   add_settings(time_limit = 1, precision = 4, nb_solutions = 2)
#' # print problem
#' print(p)
#' }
#' @export
add_settings <- function(problem, precision = 4, time_limit = 0,
                         nb_solutions = 1, optimality_gap = 0) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem"),
    assertthat::is.count(precision),
    assertthat::noNA(precision),
    assertthat::is.count(time_limit + 1),
    assertthat::noNA(time_limit),
    assertthat::is.count(nb_solutions),
    assertthat::noNA(nb_solutions),
    assertthat::is.number(optimality_gap),
    assertthat::noNA(optimality_gap),
    optimality_gap >= 0,
    optimality_gap <= 1
  )

  # add settings
  problem$settings <- list(
    precision = as.integer(precision),
    time_limit = as.integer(time_limit),
    nb_solutions = as.integer(nb_solutions),
    optimality_gap = optimality_gap
  )

  # return updated problem
  problem
}
