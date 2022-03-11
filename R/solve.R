#' @include internal.R
NULL

#' Solve a restoration optimization problem
#'
#' Solve a restoration optimization problem to generate a solution.
#'
#' @details This function relies on the Choco-solver (https://choco-solver.org/)
#' to solve a restoration optimization problem. If the solver finds a solution,
#' it outputs a raster with 5 possible values:
#'   - NA: corresponds to the NA (or NO_DATA) values in the input habitat raster.
#'     -1: corresponds to non-habitat areas that were locked out.
#'   - 0:  corresponds to non-habitat areas that were available for selection.
#'     1:  corresponds to habitat areas.
#'     2:  corresponds to selected planning units for restoration.
#' If the solve function return an no-solution error, it is either because the
#' solver could not find a solution within the time limit that was set
#' (see \link{add_settings}), or because the solver has detected that this is
#' not possible to satisfy the constraints (the constraints are contradictory).
#' In the first case, you can try to increase the time limit.
#' In the second case, you should modify your targets.
#'
#' @param a [restopt_problem()] Restoration problem object.
#'
#' @param b Argument not used.
#'
#' @param verbose if TRUE, output solver logs. (FALSE by default)
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
#'   col = c("#E5E5E5", "#fff1d6", "#b2df8a", "#1f78b4")
#' )
#' }
#'
#' @export
solve.RestoptProblem <- function(a, b, ...) {
  # assert argument is valid
  assertthat::assert_that(inherits(a, "RestoptProblem"))
  args <- list(...)
  if ("verbose" %in% names(args)) {
    assertthat::is.flag(args$verbose)
    verbose <- args$verbose
  } else {
    verbose <- FALSE
  }
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
      output_path,
      verbose
    ),
    silent = TRUE
  )

  # throw error if failed
  if (inherits(result, "try-error")) {
    .jgc()
    stop(paste("failed to complete optimization\n\t", result))
  }

  status <- jproblem$getSearchState()

  # Indicate if the solver did not find solution
  if (result == FALSE) {
    .jgc()
    if (status == "TERMINATED") {
      stop(paste("There is no solution, please adjust your targets\n"))
    }
    if (status == "STOPPED") {
      stop(paste("The research was stopped before a solution was found,",
                 " consider increasing the time limit\n"))
    }
  }

  # import results
  r <- terra::rast(paste0(output_path, ".tif"))
  attributes(r)$metadata <- utils::read.csv(paste0(output_path, ".csv"))

  solving_time <- attributes(r)$metadata$solving.time..ms.

  # If the solver found a solution, and if an optimization objective was
  # defined, indicate whether it was proven optimal, or if it is the best
  # solution found within the time limit but not proven optimal
  if (!inherits(a$objective, "NoObjective")) {
    if (status == "TERMINATED") {
      cat(crayon::green(paste("Good news: the solver found a solution statisfying",
                              "the constraints that was proven optimal !",
                              "(solving time =", solving_time / 1000 ,"s)\n")))
    }
    if (status == "STOPPED") {
      cat(crayon::yellow(paste("Note: The current solution is the best that the",
                               "solver could find within the time limit.",
                               "However, the solver had not enough to prove",
                               "whether it is optimal or not. Consider increasing",
                               "the time limit if you need a better solution",
                               "(solving time =", solving_time / 1000 ,"s)\n")))
    }
  } else {
      cat(crayon::green(paste("Good news: the solver found a solution satisfying",
                              "the constraints ! (solving time =",
                              solving_time / 1000 ,"s)\n")))
  }

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
  .jgc()
  return(r)

}
