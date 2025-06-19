#' @include internal.R
NULL

#' Add constraint to forbid the creation of new patches.
#'
#' Add constraint to a restoration problem ([restopt_problem()]) object
#' to forbid the creation to new patches. All restored areas must be connected
#' to an existing habitat area.
#'
#' @inheritParams set_max_mesh_objective
#' @inherit set_max_mesh_objective return
#'
#' @details A patch is a spatially continuous set of habitat and restoration
#' planning units. This constraints applies on the union of the set of planning
#' units that are selected for restoration and the initial habitat patches,
#' and forbids the creation of new patches. All areas selected for restoration
#' must be connected to initially existing habitat areas. This constraint is
#' particularly useful for fragmentation reduction scenarios, where most of
#' the time it is desirable to extend and merge existing patches, instead of
#' creating new ones. In such cases, it also reduces the combinatorial
#' complexity by filtering areas that cannot be connected to existing patches.
#'
#' @family constraints
#'
#' @examples
#' \dontrun{
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
#'   add_restorable_constraint(
#'     min_restore = 10,
#'     max_restore = 100,
#'   ) %>%
#'   add_no_new_patch_constraint()
#'
#' # plot preprocessed data
#' plot(rast(list(p$data$existing_habitat, p$data$restorable_habitat)), nc = 2)
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
add_no_new_patch_constraint <- function(problem) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem")
  )

  # add constraint
  add_restopt_constraint(
    problem = problem,
    constraint = restopt_component(
      name = paste0(
        "no_new_patch"
      ),
      class = c("NoNewPatchConstraint", "RestoptConstraint"),
      post = function(jproblem) {
        rJava::.jcall(
          jproblem, "V", "postNoNewPatchConstraint"
        )
      }
    )
  )
}
