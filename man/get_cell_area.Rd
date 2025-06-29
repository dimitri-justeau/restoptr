% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/restopt_problem.R
\name{get_cell_area}
\alias{get_cell_area}
\title{Retrieve the aggregated cell area of a restopt problem.}
\usage{
get_cell_area(problem)
}
\arguments{
\item{problem}{\code{\link[=restopt_problem]{restopt_problem()}} Restoration problem object.}
}
\value{
\code{\link[terra:rast]{terra::rast()}} The aggregated cell area of the restopt problem.
}
\description{
Retrieve the aggregated cell area of a restopt problem.
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

get_cell_area(problem)
}

}
