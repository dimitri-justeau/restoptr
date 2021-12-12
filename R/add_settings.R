#' @include internal.R
NULL

#' Add settings
#'
#' Add settings to a restoration problem ([restore_problem()] object
#' to customize the optimization procedure.
#'
#' @param precision `numeric` Precision for calculations.
#' Defaults to 4.
#'
#' @param time_limit `integer` Maximum permitted run time for optimization
#' (seconds).
#' Defaults to 0.
#'
#' @examples
#' # TODO
#'
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
