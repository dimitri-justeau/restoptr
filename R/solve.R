#' @include internal.R
NULL

#' Solve a restoration optimization problem
#'
#' Solve a restoration optimization problem to generate a solution.
#'
#' @details This function relies on the Choco-solver (https://choco-solver.org/)
#' to solve a restoration optimization problem. If the solver finds a solution,
#' it outputs a raster with 5 possible values:
#'    NA : NA (or NO_DATA) areas from the input habitat raster.
#'     0 : non-habitat areas that were locked out.
#'     1 : non-habitat areas that were available for selection.
#'     2 : habitat areas.
#'     3 : selected planning units for restoration.
#' If the solve function return a no-solution error, it is either because the
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
#' @param ... Additional arguments:
#' `verbose`: if TRUE, output solver logs. (FALSE by default)
#'
#' @return A [restopt_solution()] object.
#'
#' @examples
#' \dontrun{
#' # load data
#' habitat_data <- rast(
#'   system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
#' )
#'
#' available <- vect(
#'   system.file("extdata", "accessible_areas.gpkg", package = "restoptr")
#' )
#'
#' # create problem with locked out constraints
#' p <- restopt_problem(
#'     existing_habitat = habitat_data,
#'     aggregation_factor = 16,
#'     habitat_threshold = 0.7
#'   ) %>%
#'   set_max_mesh_objective() %>%
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
  ac_on_disk <- terra_on_disk(a$data$locked_out)
  ## save rasters to disk if needed
  eh_data <- terra_force_disk(round(a$data$existing_habitat), datatype="INT4S")
  rh_data <- terra_force_disk(a$data$restorable_habitat)
  # Force NODATA values of locked out raster to avoid terra writing them as 0
  a$data$locked_out[is.na(a$data$locked_out)] <- -9999
  ac_data <- terra_force_disk(a$data$locked_out, NAflag = -9999)

  # import data
  # jdata <- rJava::.jnew(
  #   "org.restopt.DataLoader",
  #   terra::sources(eh_data)[[1]],
  #   terra::sources(ac_data)[[1]],
  #   terra::sources(rh_data)[[1]]
  # )
  jdata <- rJava::.jnew(
    "org.restopt.DataLoader",
    .jarray(as.integer(as.vector(a$data$existing_habitat))),
    .jarray(as.integer(as.vector(a$data$locked_out))),
    .jarray(as.vector(a$data$restorable_habitat)),
    .jarray(as.integer(as.vector(a$data$cell_area))),
    as.integer(ncol(a$data$existing_habitat)),
    as.integer(nrow(a$data$existing_habitat)),
    NaN
  )

  # initialize problem
  jproblem <-rJava::.jnew(
    "org.restopt.RestoptProblem", jdata,
    0L
  )

  # add constraints
  for (i in seq_along(a$constraints)) {
    a$constraints[[i]]$post(jproblem)
  }

  # add objective and solve the problem
  t <- Sys.time()
  result <- try(
    a$objective$post(
      jproblem,
      a$settings$nb_solutions,
      a$settings$precision,
      a$settings$time_limit,
      verbose
    ),
    silent = TRUE
  )
  solving_time <- difftime(Sys.time(), t, units = "secs")

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
  nb_sols <- result$size()
  solutions <- sapply(seq(0, nb_sols - 1), function(i) {
    sol <- result$get(as.integer(i))
    pus <- sol$getRestorationPlanningUnitsCompleteIndex() + 1 # Java arrays are 0-based
    r <- round(a$data$existing_habitat) + 1
    r[r == 1 & a$data$locked_out == 1] <- 0
    r[pus] <- 3
    m <- sol$getCharacteristicsAsCsv()
    metadata <- read.csv(text = .jstrVal(m))
    restopt_solution(a, r, metadata, id_solution = i)
  })

  # If the solver found a solution, and if an optimization objective was
  # defined, indicate whether it was proven optimal, or if it is the best
  # solution found within the time limit but not proven optimal
  if (!inherits(a$objective, "NoObjective")) {
    if (status == "TERMINATED") {
      cat(crayon::green(paste("Good news: the solver found", nb_sols ,"solution(s) statisfying",
                              "the constraints that was proven optimal !",
                              "(solving time =", solving_time, "s)")), "\n")
    }
    if (status == "STOPPED") {
      cat(crayon::yellow(paste("Note: The current solution is the best that the",
                               "solver could find within the time limit.",
                               "However, the solver had not enough to prove",
                               "whether it is optimal or not. Consider increasing",
                               "the time limit if you need a better solution",
                               "(solving time =", solving_time, "s)")), "\n")
    }
  } else {
      cat(crayon::green(paste("Good news: the solver found", nb_sols ,"solution(s) satisfying",
                              "the constraints ! (solving time =",
                              solving_time, "s)")), "\n")
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
  if (length(solutions) == 1) {
    return(solutions[[1]])
  }
  return(solutions)
}
