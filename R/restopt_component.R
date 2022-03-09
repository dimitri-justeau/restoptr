#' @include internal.R
NULL

#' Restoration problem component
#'
#' Create a component to add to a restoration optimization problem.
#'
#' @param name `character` Name of the component.
#'
#' @param class `character` Vector of classes to identify the component.
#'
#' @param post `function` Function used to update the Java representation
#'   of the problem (see details for more information).
#'
#' @details
#' The `post` argument should contain a function. Components used
#' to add constraints should have a `post` function that contains a single
#' parameter called `jproblem`. Additionally, components used to add
#' objectives should have a `post` function that contains the following
#' parameters: `jproblem`, `precision`, `time_limit`, and `output_path`.
#'
#' @return A `RestoptComponent` object.
#'
#' @noRd
restopt_component <- function(name, class, post) {
  # assert that arguments are valid
  assertthat::assert_that(
    assertthat::is.string(name),
    assertthat::noNA(name),
    assertthat::validate_that(all(sapply(class, assertthat::is.string))),
    assertthat::noNA(class),
    is.function(post)
  )

  # return object
  structure(
    list(
      name = name,
      post = post
    ),
    class = c(class, "RestoptComponent")
  )
}
