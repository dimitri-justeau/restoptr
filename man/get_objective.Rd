% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/restopt_problem.R
\name{get_objective}
\alias{get_objective}
\title{Retrieve the optimization objective of a restopt problem.}
\usage{
get_objective(problem)
}
\arguments{
\item{problem}{\code{\link[=restopt_problem]{restopt_problem()}} Restoration problem object.}
}
\value{
\code{RestoptObjectve} The optimization objective of the restopt problem.
}
\description{
Retrieve the optimization objective of a restopt problem.
}
\examples{
\dontrun{
#' # load data
habitat_data <- rast(
  system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
)

# create problem
problem <- restopt_problem(
       existing_habitat = habitat_data,
       aggregation_factor = 4,
       habitat_threshold = 0.7
)

get_objective(problem)
}

}
