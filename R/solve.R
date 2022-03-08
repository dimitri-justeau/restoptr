#' @include internal.R
NULL

#' Solve a restoration optimization problem
#'
#' Solve a restoration optimization problem to generate a solution.
#'
#' @param a [restopt_problem()] Restoration problem object.
#'
#' @param b Argument not used.
#'
#' @param ... Unused arguments.
#'
#' @return A [terra::rast()] object.
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
solve.RestoptProblem <- function(a, b, ...) {
  # assert argument is valid
  assertthat::assert_that(inherits(a, "RestoptProblem"))

  # prepare data
  ## check if data already saved to disk
  eh_on_disk <- terra_on_disk(a$data$existing_habitat)
  rh_on_disk <- terra_on_disk(a$data$restorable_habitat)
  ac_on_disk <- terra_on_disk(a$data$locked_out$data)
  ## save rasters to disk if needed
  eh_data <- terra_force_disk(a$data$existing_habitat)
  rh_data <- terra_force_disk(a$data$restorable_habitat)
  # Force NODATA values of locked out raster to avoid terra writing them as 0
  a$data$locked_out$data[is.na(a$data$locked_out$data)] <- -9999
  ac_data <- terra_force_disk(a$data$locked_out$data)

  # import data
  jdata <- rJava::.jnew(
    "org.restopt.DataLoader",
    terra::sources(eh_data)[[1]],
    terra::sources(ac_data)[[1]],
    terra::sources(rh_data)[[1]]
  )

  # initialize problem
  jproblem <-rJava::.jnew(
    "org.restopt.BaseProblem", jdata,
    0L
  )

  # add constraints
  for (i in seq_along(a$constraints)) {
    a$constraints[[i]]$post(jproblem)
  }

  # add objective and solve the problem
  output_path <- tempfile()
  result <- try(
    a$objective$post(
      jproblem,
      a$settings$precision,
      a$settings$time_limit,
      output_path
    ),
    silent = TRUE
  )

  # throw error if failed
  if (inherits(result, "try-error")) {
    stop(paste("failed to complete optimization\n\t", result))
  }

  # import results
  r <- terra::rast(paste0(output_path, ".tif"))
  attributes(r)$metadata <- utils::read.csv(paste0(output_path, ".csv"))

  # clean up
  if (!eh_on_disk) {
    unlink(terra::sources(eh_data)[[1]])
    rm(eh_data)
  }
  if (!rh_on_disk) {
    unlink(terra::sources(rh_data)[[1]])
    rm(rh_data)
  }
  if (!ac_on_disk) {
    unlink(terra::sources(ac_data)[[1]])
    rm(ac_data)
  }

  return(r)

}
