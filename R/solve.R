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
solve.RestoptProblem <- function(problem, ...) {
  # assert argument is valid
  assertthat::assert_that(inherits(problem, "RestoptProblem"))

  # prepare data
  ## check if data already saved to disk
  eh_on_disk <- terra_on_disk(problem$data$existing_habitat)
  rh_on_disk <- terra_on_disk(problem$data$restorable_habitat)
  ac_on_disk <- terra_on_disk(problem$data$locked_out$data)
  ## save rasters to disk if needed
  eh_data <- terra_force_disk(problem$data$existing_habitat)
  rh_data <- terra_force_disk(problem$data$restorable_habitat)
  ac_data <- terra_force_disk(problem$data$locked_out$data)

  # import data
  jdata <- rJava::.jnew(
    "org.restopt.DataLoader",
    terra::sources(eh_data)[[1]],
    terra::sources(ac_data)[[1]],
    terra::sources(rh_data)[[1]]
  )
  # initialize problem
  jproblem <-rJava::.jnew(
    "org.restopt.BaseProblem", jdata, as.integer(problem$data$locked_out$raster_value)
  )

  # add constraints
  for (i in seq_along(problem$constraints)) {
    problem$constraints[[i]]$post(jproblem)
  }

  # add objective and solve the problem
  output_path <- tempfile()
  result <- try(
    problem$objective$post(
      jproblem, problem$settings$precision, problem$settings$time_limit, output_path
    ),
    silent = TRUE
  )

  # throw error if failed
  if (inherits(result, "try-error")) {
    stop("failed to complete optimization")
  }

  # import results
  r <- terra::rast(paste0(output_path, ".tif"))
  attributes(r)$metadata <- utils::read.csv(paste0(output_path, ".csv"))

  # clean up
  if (eh_on_disk) {
    unlink(terra::sources(eh_data)[[1]])
    rm(eh_data)
  }
  if (rh_on_disk) {
    unlink(terra::sources(rh_data)[[1]])
    rm(rh_data)
  }
  if (ac_on_disk) {
    unlink(terra::sources(ac_data)[[1]])
    rm(ac_data)
  }

  return(r)
}
