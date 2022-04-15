#' @include internal.R
NULL

#' Is Java is available?
#'
#' The \pkg{restoptr} package uses Java to perform optimization procedures.
#' As such, it is important that Java is installed correctly when using this
#' package. This function verifies if Java is available on the system.
#'
#' @return
#' A `logical` (`TRUE` or `FALSE`) value indicating if Java is available
#' for usage.
#'
#' @export
is_java_available <- function() {
  inherits(
    try(
      rJava::.jcall(
        "java/lang/System", "S", "getProperty", "java.runtime.version"
      ),
      silent = TRUE
    ),
    "character"
  )
}
