#' @include internal.R
NULL

#' Set an objective to minimize the amount restoration area.
#'
#' Specify that a restoration problem ([restopt_problem()]) should minimize
#' the restoration area needed to reach the habitat proportion threshold
#' specified in the problem description.
#'
#' @param problem [restopt_problem()] Restoration problem object.
#'
#' @details The restoration area corresponds to the minimum amount of area that
#' must be restored in the selected planning units to reach the minimum habitat
#' proportion threshold specified in the problem description,
#'
#' @return An updated restoration problem ([restopt_problem()]) object.
#'
#' @family objectives
#'
#' @examples
#' \donttest{
#' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#'
#' locked_out_data <- rast(
#'  system.file("extdata", "locked_out.tif", package = "restoptr")
#' )
#'
#' # plot data
#' plot(rast(list(habitat_data, locked_out_data)), nc = 2)
#'
#' # create problem with locked out constraints
#' p <- restopt_problem(
#'     existing_habitat = habitat_data,
#'     aggregation_factor = 16,
#'     habitat_threshold = 0.7
#'   ) %>%
#'   set_min_restore_objective() %>%
#'   add_restorable_constraint(
#'     min_restore = 5,
#'     max_restore = 5,
#'   ) %>%
#'   add_locked_out_constraint(data = locked_out_data) %>%
#'   add_settings(time_limit = 1)
#'
#' # print problem
#' print(p)
#'
#' # solve problem
#' s <- solve(p)
#'
#' # plot solution
#' plot(s)
#' }
#'
#' @export
set_min_restore_objective <- function(problem) {
  # assert argument is valid
  assertthat::assert_that(inherits(problem, "RestoptProblem"))

  # set objective
  set_restopt_objective(
    problem = problem,
    objective = restopt_component(
      name = "Minimize restoration area",
      class = c("MinRestoreObjective", "RestoptObjectve"),
      post = function(jproblem, nb_solutions, precision, time_limit, optimality_gap,
                      verbose=FALSE, search_strategy="") {
        rJava::.jcall(
          jproblem, "Ljava/util/List;", "minimizeMinRestore",
          get_habitat_threshold(problem), nb_solutions, time_limit, optimality_gap,
          verbose, search_strategy
        )
      }
    )
  )
}
