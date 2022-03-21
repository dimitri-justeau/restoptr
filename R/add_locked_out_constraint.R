#' @include internal.R
NULL

#' Add locked out constraint
#'
#' Add constraint to a restoration problem ([restopt_problem()]) object
#' to specify that certain planning units cannot be selected
#' for restoration activities.
#'
#' @inheritParams set_max_mesh_objective
#'
#' @param data [terra::rast()]  or [terra::vect()] Either a raster object
#' containing binary values hat indicate which planning units cannot be selected
#' for any restoration (i.e., cells with a value equal one are locked out from
#' the solution), or a vector object whose features correspond to the locked
#' out areas. See the function `add_available_areas_constraint()` to get a locked
#' out constraint from allowed restoration areas.
#'
#' @details
#' Locked out constraints can be used to incorporate a wide range of
#' criteria into restoration planning problems.
#' They can be used to account for existing land-use practices,
#' feasibility of restoration activities, and stakeholder preferences.
#' For example, locked out constraints can be used to
#' ensure that urban areas are not selected for restoration.
#' Additionally, if restoration activities can only be implemented depending
#' on certain conditions -- such as places where landscape slope is not
#' too steep -- then locked out constraints could be used to ensure
#' restoration activities are not prioritized for places where they
#' could not be implemented.
#' Furthermore, if stakeholders require solutions that do not prioritize
#' particular places for restoration, then locked out constraints
#' can also be used to achieve this.
#' See `add_available_areas_constraint()`, which achieve the same as the locked
#' out constraint by defining areas that ARE available for restoration.
#'
#' @examples
#' \dontrun{
#' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
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
#'   set_max_iic_objective() %>%
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
#' @export
add_locked_out_constraint <- function(problem, data) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(problem, "RestoptProblem"),
    inherits(data, "SpatRaster") || inherits(data, "SpatVector")
  )
  ## further checks
  if (inherits(data, "SpatRaster")) {
    assertthat::assert_that(
      terra::hasValues(data)
    )
    original_res <- terra::compareGeom(
      problem$data$habitat_original, data, stopiffalse = FALSE
    )
    if (original_res) {
      down_sum <- terra::aggregate(
        data,
        fact = problem$data$aggregation_factor,
        fun = "sum",
        na.rm = TRUE
      )
      data <- (down_sum / problem$data$cell_area) >= problem$data$habitat_threshold
    } else {
      assertthat::assert_that(
        terra::compareGeom(
          problem$data$existing_habitat, data, stopiffalse = FALSE
        ) || original_res,
        msg = paste(
          "argument to \"data\" has different spatial properties to",
          "the \"existing_habitat\" and \"original_habitat\" data in",
          "the problem"
        )
      )
    }
  } else {

    data <- rasterize(data, problem$data$existing_habitat, background = 0, touches = TRUE)
  }
  problem$data$locked_out <- data

  src_locked_out <- basename(terra::sources(data)[[1]])

  add_restopt_constraint(
    problem = problem,
    constraint = restopt_component(
      name = paste0(
        "locked out (",
        "data = ", ifelse(src_locked_out != "", src_locked_out, "in memory"),
        ")"
      ),
      class = c("LockedOutConstraint", "RestoptConstraint"),
      post = function(jproblem) {} # nothing to do here
    )
  )
}
