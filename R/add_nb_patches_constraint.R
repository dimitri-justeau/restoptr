#' @include internal.R
NULL

#' Add constraint to limit the number of patches (habitat + restoration)
#'
#' Add constraint to a restoration problem ([restopt_problem()]) object
#' to specify the number of patches (habitat + restoration) that can be
#' present within a solution.
#'
#' @inheritParams set_max_mesh_objective
#' @inherit set_max_mesh_objective return
#'
#' @param min_nb_patches `integer` Minimum number of patches.
#'
#' @param max_nb_patches `integer` Maximum number of patches.
#'
#' @details A patch is a spatially continuous set of habitat and restoration
#' planning units. This constraints applies on the union of the set of planning
#' units that are selected for restoration and the initial habitat patches,
#' and allows to specify a minimum and maximum number of patches. In practice,
#' this constraint is useful to set simple targets for structural connectivity.
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
#'   add_nb_patches_constraint(1, 1)
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
add_nb_patches_constraint <- function(problem,
                                      min_nb_patches,
                                      max_nb_patches) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem"),
    assertthat::is.count(min_nb_patches),
    assertthat::noNA(min_nb_patches),
    assertthat::is.count(max_nb_patches),
    assertthat::noNA(max_nb_patches),
    isTRUE(max_nb_patches >= min_nb_patches)
  )

  # coerce arguments to integer
  min_nb_patches <- as.integer(min_nb_patches)
  max_nb_patches <- as.integer(max_nb_patches)

  # add constraint
  add_restopt_constraint(
    problem = problem,
    constraint = restopt_component(
      name = paste0(
        "nb_patches (",
        "min_nb_patches = ", min_nb_patches,
        ", max_nb_patches = ", max_nb_patches, ")"
      ),
      class = c("NbPatchesConstraint", "RestoptConstraint"),
      post = function(jproblem) {
        rJava::.jcall(
          jproblem, "V", "postNbPatchesConstraint",
          min_nb_patches, max_nb_patches
        )
      }
    )
  )
}
