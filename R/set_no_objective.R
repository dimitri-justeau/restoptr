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
#' ) %>% set_no_objective()
#'
#' # print problem
#' print(p)
#'
#' # Solve problem
#' s <- solve(p)
#' #' # plot solution
#' plot(
#'   x = s, main = "solution",
#'   col = c("#E5E5E5", "#fff1d6", "#b2df8a", "#1f78b4")
#' )
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
      post = function(jproblem, precision, time_limit, output_path, verbose=FALSE) {
        rJava::.jcall(
          jproblem, "Z", "findSolution", output_path, time_limit, verbose
        )
      }
    )
  )
}