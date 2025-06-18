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
#' `verbose` (logical) if TRUE, output solver logs. (FALSE by default)
#' `search_strategy` (character) specify the solver's search strategy, among:
#'                            "" -> default
#'                            "RANDOM",
#'                            "DOM_OVER_W_DEG",
#'                            "DOM_OVER_W_DEG_REF",
#'                            "MIN_DOM_LB",
#'                            "MIN_DOM_UB",
#'                            "ACTIVITY_BASED",
#'                            "CONFLICT_HISTORY",
#'                            "FAILURE_RATE",
#'                            "FAILURE_LENGTH"
#' `lns` (logical) if TRUE, activate Large Neighborhood Search (LNS). LNS
#'    can boost the optimization efficiency for large problems, with the
#'    price of less guarantees. Warning: it is highly recommended to set a
#'    time limit when using LNS because optimality proof may become unreachable
#'    (it depends on the characteristics of the problem), causing the solver to
#'    run forever without a time limit. Also, while LNS may boost the solver,
#'    it is very dependent on the problem's setting and size. It is recommended
#'    to first try solving a problem without LNS, and then try it if the
#'    results are not satisfactory.
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
#' plot(s)
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

  if ("search_strategy" %in% names(args)) {
    assertthat::is.string(args$search_strategy)
    search_strategy <- args$search_strategy
  } else {
    search_strategy <- ""
  }

  if ("lns" %in% names(args)) {
    assertthat::is.flag(args$lns)
    lns <- args$lns
  } else {
    lns <- FALSE
  }

  if (a$data$aggregation_method == "lossless") {
    agg_factor <- as.integer(a$data$aggregation_factor)
  } else {
    agg_factor <- 1L
  }

  jdata <- rJava::.jnew(
    "org.restopt.DataLoader",
    .jarray(as.integer(as.vector(a$data$existing_habitat))),
    .jarray(as.integer(as.vector(a$data$locked_out))),
    .jarray(as.vector(as.double(a$data$restorable_habitat))),
    .jarray(as.integer(as.vector(a$data$cell_area))),
    as.integer(ncol(a$data$existing_habitat)),
    as.integer(nrow(a$data$existing_habitat)),
    NaN
  )

  # initialize problem
  jproblem <-rJava::.jnew(
    "org.restopt.RestoptProblem", jdata,
    0L, agg_factor
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
      a$settings$optimality_gap,
      verbose,
      search_strategy,
      lns
    ),
    silent = TRUE
  )
  solving_time <- round(difftime(Sys.time(), t, units = "secs"), 2)

  # throw error if failed
  if (inherits(result, "try-error")) {
    .jgc()
    stop(paste("failed to complete optimization\n\t", result))
  }

  status <- jproblem$getSearchState()
  nb_sols <- result$size()

  # Indicate if the solver did not find solution
  if (nb_sols == 0) {
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
  solutions <- sapply(seq(0, nb_sols - 1), function(i) {
    sol <- result$get(as.integer(i))
    pus <- sol$getRestorationPlanningUnitsCompleteIndex() + 1 # Java arrays are 0-based
    r <- round(a$data$existing_habitat) + 1
    r[r == 1 & a$data$locked_out == 1] <- 0
    r[pus] <- 3
    m <- sol$getCharacteristicsAsCsv()
    metadata <- utils::read.csv(text = .jstrVal(m))
    metadata$optimality_proven <- ifelse(
      metadata$optimality_proven == "true",
      TRUE,
      FALSE
    )
    restopt_solution(a, r, metadata, id_solution = as.integer(i + 1))
  })

  proven_optimal <- get_metadata(solutions[[1]])$optimality_proven

  # If the solver found a solution, and if an optimization objective was
  # defined, indicate whether it was proven optimal, or if it is the best
  # solution found within the time limit but not proven optimal
  if (!inherits(a$objective, "NoObjective")) {
    if (proven_optimal) {
      if (nb_sols == 1) {
        message(crayon::green(paste(
          "Good news: the solver found", nb_sols ,"solution statisfying",
          "the constraints that was proven optimal !",
          "(solving time =", solving_time, "s)"
        )))
      } else {
        message(crayon::green(paste(
          "Good news: the solver found", nb_sols ,"solutions statisfying",
          "the constraints that were proven optimal !",
          "(solving time =", solving_time, "s)"
        )))
      }
      if (nb_sols != a$settings$nb_solutions) {
        if (status == "STOPPED") {
          message(crayon::yellow(paste(
            a$settings$nb_solutions, "optimal solutions",
            "were requested, however the solver only had",
            "enough time to find", nb_sols)))
        } else {
          message(crayon::yellow(paste(
            a$settings$nb_solutions, "optimal solutions",
            "were requested, however only", nb_sols,
            "optimal solution exist")))
          }
      }
    } else {
      message(crayon::yellow(paste(
        "Note: The current solution is the best that the",
        "solver could find within the time limit.",
        "However, the solver had not enough to prove",
        "whether it is optimal or not. Consider increasing",
        "the time limit if you need a better solution",
        "(solving time =", solving_time, "s)"
      )))
    }
  } else {
    if (nb_sols == 1) {
      message(crayon::green(paste(
        "Good news: the solver found", nb_sols ,"solution satisfying",
        "the constraints ! (solving time =",
        solving_time, "s)"
      )))
    } else {
      message(crayon::green(paste(
        "Good news: the solver found", nb_sols ,"solutions satisfying",
        "the constraints ! (solving time =",
        solving_time, "s)"
      )))
    }
    if (nb_sols != a$settings$nb_solutions) {
      if (status == "STOPPED") {
        message(crayon::yellow(paste(
          a$settings$nb_solutions, "solutions",
          "were requested, however the solver only had",
          "enough time to find", nb_sols
        )))
      } else {
        message(crayon::yellow(paste(
          a$settings$nb_solutions, "optimal solutions",
          "were requested, however only", nb_sols,
          "solution satisfying the constraints exist"
        )))
      }
    }
  }

  .jgc()
  if (length(solutions) == 1) {
    return(solutions[[1]])
  }
  return(solutions)
}
