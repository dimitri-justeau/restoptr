#' @include internal.R
NULL

#' Restoration problem component
#'
#' Create a component to add to a restoration optimization problem.
#'
#' @param name `character` Name of the component.
#'
#' @param java `character` Name Java method for adding component.
#'
#' @param class `character` Vector of classes to identify the component.
#'
#' @param data `list` Object containing parameters for adding the component.
#'  Defaults to an empty list.
#'
#' @param args `character` Vector used to map the names of the elements in
#'  `data` to the Java method.
#'  Defaults to an empty character vector.
#'
#' @return An object.
#'
#' @examples
#' # TODO
#'
#' @export
restopt_component <- function(name, class, data = list(), post) {
  structure(
    list(
      name = name,
      data = data,
      post = post
    ),
    class = class
  )
}
