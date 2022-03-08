#' @include internal.R
NULL

#' Add an objective to maximize effective mesh size
#'
#' Specify that a restoration problem ([restopt_problem()]) should
#' maximize effective mesh size.
#'
#' @param problem [restopt_problem()] Restoration problem object.
#'
#' @details The effective mesh size is a measure of landscape fragmentation
#' based on the probability that two randomly chosen points are located in the
#' same patch (Jaeger, 2000). Maximizing it in the context of restoration
#' favours fewer and larger patches.
#'
#' @return An updated restoration problem ([restopt_problem()] object.
#'
#' @references
#' Jaeger, J. A. G. (2000). Landscape division, splitting index, and effective
#' mesh size: New measures of landscape fragmentation. Landscape Ecology, 15(2),
#' 115‑130. https://doi.org/10.1023/A:1008129329289
#'
#' @examples
#' \dontrun{
#' #' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat.tif", package = "restoptr")
#' )
#'
#' restorable_data <- rast(
#'   system.file("extdata", "restorable.tif", package = "restoptr")
#' )
#'
#' locked_out_data <- rast(
#'  system.file("extdata", "locked-out.tif", package = "restoptr")
#' )
#'
#' # plot data
#' plot(rast(list(habitat_data, restorable_data, locked_out_data)), nc = 3)
#'
#' # create problem with locked out constraints
#' p <-
#'   restopt_problem(
#'     existing_habitat = habitat_data,
#'     restorable_habitat = restorable_data
#'   ) %>%
#'   add_max_mesh_objective() %>%
#'   add_restorable_constraint(
#'     min_restore = 5,
#'     max_restore = 5,
#'     cell_area = 1
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
#' plot(
#'   x = s, main = "solution",
#'   col = c("#E5E5E5", "#ffffff", "#b2df8a", "#1f78b4")
#' )
#' }
#'
#' @export
add_max_mesh_objective <- function(problem) {
  # assert argument is valid
  assertthat::assert_that(inherits(problem, "RestoptProblem"))

  # add objective
  add_restopt_objective(
    problem = problem,
    objective = restopt_component(
      name = "Maximize effective mesh size",
      class = c("MaxMeshObjective", "RestoptObjectve"),
      post = function(jproblem, precision, time_limit, output_path) {
        rJava::.jcall(
          jproblem, "Z", "maximizeMESH", precision, output_path, time_limit
        )
      }
    )
  )
}
