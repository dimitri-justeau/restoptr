#' @include internal.R
NULL

#' Add an objective to maximize the integral index of connectivity
#'
#' Specify that a restoration problem ([restopt_problem()]) should
#' the integral index of connectivity (IIC).
#'
#' @inheritParams add_max_mesh_objective
#'
#' @details The integral index of connectivity (IIC) is a graph-based inter-patch
#' connectivity index based on a binary connection model (Pascual-Hortal &
#' Saura, 2006). Its maximization in the context of restoration favours
#' restoring the structural connectivity between large patches. IIC is unitless
#' and comprised between 0 (no connectivity) and 1 (all the landscape is
#' habitat, thus fully connected).
#'
#' @return An updated restoration problem ([restopt_problem()] object.
#'
#' @references
#' Pascual-Hortal, L., & Saura, S. (2006).
#' Comparison and development of new graph-based landscape connectivity indices:
#' Towards the priorization of habitat patches and corridors for conservation.
#' Landscape Ecology, 21(7), 959‑967. https://doi.org/10.1007/s10980-006-0013-z
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
#'   add_max_iic_objective() %>%
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
#'   col = c("#E5E5E5", "#fff1d6", "#b2df8a", "#1f78b4")
#' )
#' }
#'
#' @export
add_max_iic_objective <- function(problem) {
  # assert argument is valid
  assertthat::assert_that(inherits(problem, "RestoptProblem"))

  # add objective
  add_restopt_objective(
    problem = problem,
    objective = restopt_component(
      name = "Maximize integral index of connectivity",
      class = c("MaxIicObjective", "RestoptObjectve"),
      post = function(jproblem, precision, time_limit, output_path) {
        rJava::.jcall(
          jproblem, "Z", "maximizeIIC", precision, output_path, time_limit
        )
      }
    )
  )
}
