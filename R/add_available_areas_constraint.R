#' @include internal.R
NULL

#' Add available areas constraint
#'
#' Add constraint to a restoration problem ([restopt_problem()]) object
#' to specify that only certain planning units can be selected for restoration
#' activities.
#'
#' @inheritParams set_max_mesh_objective
#' @inherit set_max_mesh_objective return
#'
#' @param data [terra::rast()]  or [terra::vect()] Either a raster object
#' containing binary values hat indicate which planning units can be selected
#' for restoration (i.e., only cells with a value equal one are available),
#' or a vector object whose features correspond to the available areas.
#'
#' @details
#' Available areas constraints can be used to incorporate a wide range of
#' criteria into restoration planning problems.
#' They can be used to account for existing land-use practices,
#' feasibility of restoration activities, and stakeholder preferences.
#' For example, available areas constraints can be used to
#' ensure that urban areas are not selected for restoration.
#' Additionally, if restoration activities can only be implemented depending
#' on certain conditions -- such as places where landscape slope is not
#' too steep -- then available areas constraints could be used to ensure
#' restoration activities are not prioritized for places where they
#' could not be implemented.
#' Furthermore, if stakeholders require solutions that do not prioritize
#' particular places for restoration, then available areas constraints
#' can also be used to achieve this.
#' See `add_locked_out_constraint()`, which achieve the same as available
#' areas constraint by defining areas that are NOT available for restoration.
#' **Note**: The locked out constraint and the available are the same constraints,
#' with a different input data. Thus, from a modelling perspective,
#' `add_available_areas_constraint()` is just a pre processing layer in front of
#' `add_locked_out_constraint()`. This is why if you print a restopt problem
#' with an available areas constraint, you will see a locked out constraint.
#'
#' @family constraints
#'
#' @examples
#' \donttest{
#' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#'
#' available <- vect(
#' system.file("extdata", "accessible_areas.gpkg", package = "restoptr")
#' )
#'
#' # plot data
#' plot(habitat_data)
#' plot(available, add=TRUE)
#'
#' # create problem with available areas constraints
#' p <- restopt_problem(
#'     existing_habitat = habitat_data,
#'     aggregation_factor = 16,
#'     habitat_threshold = 0.7
#'   ) %>%
#'   set_max_iic_objective() %>%
#'   add_restorable_constraint(
#'     min_restore = 5,
#'     max_restore = 5,
#'   ) %>%
#'   add_available_areas_constraint(available) %>%
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
add_available_areas_constraint <- function(problem, data) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem"),
    inherits(data, "SpatRaster") || inherits(data, "SpatVector")
  )
  ## further checks
  if (inherits(data, "SpatRaster")) {
    data <- round(data!=1)
    add_locked_out_constraint(problem, data)
  } else {
    data <- invert_vector(data, extent = ext(problem$data$existing_habitat))
    add_locked_out_constraint(
      problem,
      data
    )
  }
}
