#' @include internal.R
NULL

#' Configure the solver to only satisfy the constraints, without optimization
#' objective
#'
#' Specify that a restoration problem ([restopt_problem()]) should satisfy
#' the constraints without optimization objective.
#'
#' @param problem [restopt_problem()] Restoration problem object.
#'
#' @details Using set_no_objective() in a restopt problem, the solver will
#' return the first solution found satisfying the constraint, without any
#' optimization objective. This "no objective" setting is set by default
#' when creating a restopt problem.
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
#' # create problem
#' p <- restopt_problem(
#'     existing_habitat = habitat_data,
#'     aggregation_factor = 16,
#'     habitat_threshold = 0.7
#'   ) %>% set_no_objective()
#'
#' # print problem
#' print(p)
#'
#' # Solve problem
#' s <- solve(p)
#' # plot solution
#' plot(s)
#' }
#'
#' @export
set_no_objective <- function(problem) {
  # assert argument is valid
  assertthat::assert_that(inherits(problem, "RestoptProblem"))
  # add objective
  set_restopt_objective(
    problem = problem,
    objective = restopt_component(
      name = "No optimization objective",
      class = c("NoObjective", "RestoptObjectve"),
      post = function(jproblem, nb_solutions, precision, time_limit, optimality_gap, verbose=FALSE) {
        rJava::.jcall(
          jproblem, "Ljava/util/List;", "findSolutions", nb_solutions, time_limit, verbose
        )
      }
    )
  )
}
