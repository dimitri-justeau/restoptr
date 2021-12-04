#' @include internal.R
NULL

#' Solve a restoration optimization problem
#'
#' Solve a restoration optimization problem to generate a solution.
#'
# @inheritParams add_max_mesh_objective
#'
#' @return A [terra::rast()] object.
#'
#' @examples
#' # TODO
#'
#' @export
solve.RestoptProblem <- function(a, b, ...) {
  # assert argument is valid
  assertthat::assert_that(inherits(a, "RestoptProblem"))

  # assert argument is valid
  assertthat::assert_that(inherits(x, "RestoptProblem"))

  # prepare data
  ## check if data already saved to disk
  eh_on_disk <- terra_on_disk(x$data$existing_habitat)
  rh_on_disk <- terra_on_disk(x$data$restorable_habitat)
  ## save rasters to disk if needed
  eh_data <- terra_force_disk(x$data$existing_habitat)
  rh_data <- terra_force_disk(x$data$existing_habitat)

  # import data
  jdata <- rJava::.jnew(
    "org.restopt.DataLoader",
    problem@habitat_path,
    problem@accessible_path,
    problem@restorable_path
  )
  # initialize problem
  jproblem <-rJava::.jnew(
    "org.restopt.BaseProblem", jdata, problem@accessible_value
  )

  # add constraints
  for (i in seq_along(x$constraints)) {
    x$constraints[[i]]$post(jproblem)
  }

  # add objective and solve the problem
  output_path <- tempfile()
  result <- try(
    x$objective$post(
      jproblem, x$settings$precision, x$settings$time_limit, output_path
    ),
    silent = TRUE
  )

  # throw error if failed
  if (inherits(result, "try-error")) {
    stop("failed to complete optimization")
  }

  # import results
  r <- terra::rast(paste0(output_path, ".tif"))
  attributes(r)$metadata <- utils::read.csv(paste0(output, ".csv"))

  # clean up
  if (eh_on_disk) {
    rm(eh_data)
    unlink(terra::sources(eh_data)$source[[1]])
  }
  if (rh_on_disk) {
    rm(rh_data)
    unlink(terra::sources(rh_data)$source[[1]])
  }


}
