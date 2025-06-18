#' @include internal.R
NULL

#' Set an objective to maximize the number of planning units
#'
#' Specify that a restoration problem ([restopt_problem()]) should
#' maximize the number of planning units.
#'
#' @param problem [restopt_problem()] Restoration problem object.
#'
#' @details Planning units correspond to aggregated cells from the original
#' dataset. Maximizing the number of planning units increased the spatial extent
#' of the restoration area. This can be useful when the budget is limited and
#' the aim is to restore the larget possible extent.
#'
#' @return An updated restoration problem ([restopt_problem()]) object.
#'
#' @family objectives
#'
#' @examples
#' \dontrun{
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
#'   set_max_nb_pus_objective() %>%
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
set_max_nb_pus_objective <- function(problem) {
  # assert argument is valid
  assertthat::assert_that(inherits(problem, "RestoptProblem"))

  # set objective
  set_restopt_objective(
    problem = problem,
    objective = restopt_component(
      name = "Maximize number of planning units",
      class = c("MaxNbPusObjective", "RestoptObjectve"),
      post = function(jproblem, nb_solutions, precision, time_limit, optimality_gap,
                      verbose=FALSE, search_strategy="", lns=FALSE) {
        rJava::.jcall(
          jproblem, "Ljava/util/List;", "maximizeNbPUS", nb_solutions, time_limit,
          optimality_gap, verbose, search_strategy, lns
        )
      }
    )
  )
}
