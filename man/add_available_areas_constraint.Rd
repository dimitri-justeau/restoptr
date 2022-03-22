% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_available_areas_constraint.R
\name{add_available_areas_constraint}
\alias{add_available_areas_constraint}
\title{Add available areas constraint}
\usage{
add_available_areas_constraint(problem, data)
}
\arguments{
\item{problem}{\code{\link[=restopt_problem]{restopt_problem()}} Restoration problem object.}

\item{data}{\code{\link[terra:rast]{terra::rast()}}  or \code{\link[terra:vect]{terra::vect()}} Either a raster object
containing binary values hat indicate which planning units can be selected
for restoration (i.e., only cells with a value equal one are available),
or a vector object whose features correspond to the available areas.}
}
\description{
Add constraint to a restoration problem (\code{\link[=restopt_problem]{restopt_problem()}}) object
to specify that only certain planning units can be selected for restoration
activities.
}
\details{
Available areas constraints can be used to incorporate a wide range of
criteria into restoration planning problems.
They can be used to account for existing land-use practices,
feasibility of restoration activities, and stakeholder preferences.
For example, available areas constraints can be used to
ensure that urban areas are not selected for restoration.
Additionally, if restoration activities can only be implemented depending
on certain conditions -- such as places where landscape slope is not
too steep -- then available areas constraints could be used to ensure
restoration activities are not prioritized for places where they
could not be implemented.
Furthermore, if stakeholders require solutions that do not prioritize
particular places for restoration, then available areas constraints
can also be used to achieve this.
See \code{add_locked_out_constraint()}, which achieve the same as available
areas constraint by defining areas that are NOT available for restoration.
\strong{Note}: The locked out constraint and the available are the same constraints,
with a different input data. Thus, from a modelling perspective,
\code{add_available_areas_constraint()} is just a pre processing layer in front of
\code{add_locked_out_constraint()}. This is why if you print a restopt problem
with an available areas constraint, you will see a locked out constraint.
}
\examples{
\dontrun{
# load data
habitat_data <- rast(
  system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
)

available <- vect(
system.file("extdata", "accessible_areas.gpkg", package = "restoptr")
)

# plot data
plot(habitat_data)
plot(available, add=TRUE)

# create problem with available areas constraints
p <- restopt_problem(
    existing_habitat = habitat_data,
    aggregation_factor = 16,
    habitat_threshold = 0.7
  ) \%>\%
  set_max_iic_objective() \%>\%
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
plot(
  x = s, main = "solution",
  col = c("#E5E5E5", "#fff1d6", "#b2df8a", "#1f78b4")
)
}

}