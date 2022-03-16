#' @include internal.R
NULL

#' Add settings
#'
#' Add settings to a restoration problem ([restopt_problem()]) object
#' to customize the optimization procedure.
#'
#' @inheritParams set_max_mesh_objective
#'
#' @param precision `numeric` Precision for calculations.
#' Defaults to 4.
#'
#' @param time_limit `integer` Maximum permitted run time for optimization
#' (seconds).
#' Defaults to 0.
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
#' ) %>%
#' add_settings(time_limit = 1, precision = 4)
#' # print problem
#' print(p)
#' }
#' @export
add_settings <- function(problem, precision = 4, time_limit = 0) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem"),
    assertthat::is.number(precision),
    assertthat::noNA(precision),
    assertthat::is.count(time_limit + 1),
    assertthat::noNA(time_limit)
  )

  # add settings
  problem$settings <- list(
    precision = as.integer(precision),
    time_limit = as.integer(time_limit)
  )

  # return updated problem
  problem
}
