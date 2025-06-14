% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/solve.R
\name{solve.RestoptProblem}
\alias{solve.RestoptProblem}
\title{Solve a restoration optimization problem}
\usage{
\method{solve}{RestoptProblem}(a, b, ...)
}
\arguments{
\item{a}{\code{\link[=restopt_problem]{restopt_problem()}} Restoration problem object.}

\item{b}{Argument not used.}

\item{...}{Additional arguments:
\code{verbose}: if TRUE, output solver logs. (FALSE by default)
\code{search_strategy}: specify the solver's search strategy, among:
"" -> default
"RANDOM",
"DOM_OVER_W_DEG",
"DOM_OVER_W_DEG_REF",
"MIN_DOM_LB",
"MIN_DOM_UB",
"ACTIVITY_BASED",
"CONFLICT_HISTORY",
"FAILURE_RATE",
"FAILURE_LENGTH"}
}
\value{
A \code{\link[=restopt_solution]{restopt_solution()}} object.
}
\description{
Solve a restoration optimization problem to generate a solution.
}
\details{
This function relies on the Choco-solver (https://choco-solver.org/)
to solve a restoration optimization problem. If the solver finds a solution,
it outputs a raster with 5 possible values:
NA : NA (or NO_DATA) areas from the input habitat raster.
0 : non-habitat areas that were locked out.
1 : non-habitat areas that were available for selection.
2 : habitat areas.
3 : selected planning units for restoration.
If the solve function return a no-solution error, it is either because the
solver could not find a solution within the time limit that was set
(see \link{add_settings}), or because the solver has detected that this is
not possible to satisfy the constraints (the constraints are contradictory).
In the first case, you can try to increase the time limit.
In the second case, you should modify your targets.
}
\examples{
\donttest{
# load data
habitat_data <- rast(
  system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
)

available <- vect(
  system.file("extdata", "accessible_areas.gpkg", package = "restoptr")
)

# create problem with locked out constraints
p <- restopt_problem(
    existing_habitat = habitat_data,
    aggregation_factor = 16,
    habitat_threshold = 0.7
  ) \%>\%
  set_max_mesh_objective() \%>\%
  add_restorable_constraint(
    min_restore = 5,
    max_restore = 5,
  ) \%>\%
  add_available_areas_constraint(available) \%>\%
  add_settings(time_limit = 1)

# print problem
print(p)

# solve problem
s <- solve(p)

# plot solution
plot(s)
}

}
