#' @include internal.R
NULL

#' Set an objective to maximize effective mesh size
#'
#' Specify that a restoration problem ([restopt_problem()]) should
#' maximize effective mesh size.
#'
#' @param problem [restopt_problem()] Restoration problem object.
#'
#' @details The effective mesh size (MESH) is a measure of landscape fragmentation
#' based on the probability that two randomly chosen points are located in the
#' same patch (Jaeger, 2000). Maximizing it in the context of restoration
#' favours fewer and larger patches.
#'
#' @return An updated restoration problem ([restopt_problem()]) object.
#'
#' @references
#' Jaeger, J. A. G. (2000). Landscape division, splitting index, and effective
#' mesh size: New measures of landscape fragmentation. Landscape Ecology, 15(2),
#' 115‑130. https://doi.org/10.1023/A:1008129329289
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
#'   set_max_mesh_objective() %>%
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
set_max_mesh_objective <- function(problem) {
  # assert argument is valid
  assertthat::assert_that(inherits(problem, "RestoptProblem"))

  # set objective
  set_restopt_objective(
    problem = problem,
    objective = restopt_component(
      name = "Maximize effective mesh size",
      class = c("MaxMeshObjective", "RestoptObjectve"),
      post = function(jproblem, nb_solutions, precision, time_limit, optimality_gap,
                      verbose=FALSE, search_strategy="") {
        rJava::.jcall(
          jproblem, "Ljava/util/List;", "maximizeMESH", nb_solutions, precision,
          time_limit, optimality_gap, verbose, search_strategy
        )
      }
    )
  )
}
