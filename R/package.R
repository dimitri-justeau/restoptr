#' @include internal.R
NULL

#' `restoptr`: Ecological Restoration Planning
#'
#' The `restoptr` package relies on Constraint Programming (CP) to build and
#' solve ecological restoration planning problems. A `restoptr` problem starts
#' from an existing habitat (raster), where the aim is to identify optimal areas
#' that are suitable for restoration. Several constraints are available to define
#' what is expected for a suitable area for (e.g. it must be connected, compact,
#' must respect a budget, etc). Several optimization objective are also available
#' to define what is a good restoration area (e.g. it must reduce fragmentation,
#' increase ecological connectivity, minimize costs, etc.). `restoptr` relies on
#' advanced landscape indices such as the effective mesh size (Jaeger, 2000),
#' or the integral index of connectivity (Pascual-Hortal & Saura, 2006) to address
#' complex restoration planning problems.
#'
#' @details `restoptr` relies on Choco-solver (https://choco-solver.org/), an open-source
#' Java CP solver (Prud'homme et al., 2017). The computationally intensive solving
#' part is thus delegated to Java (see `restopt`, https://github.com/dimitri-justeau/restopt),
#' and the communication between R and Java is handled with the `rJava` package.
#' Therefore, a Java Runtime Environment (>= 8) is necessary to use `restopt`.
#'
#' Note that the methodology used in `restoptr` was first described in
#' Justeau-Allaire et al. (2021), but `restoptr` provides much more flexibility,
#' new features (e.g. reliable and consistent data preprocessing), new constraints,
#' new optimization objectives, and an improved computational efficiency. Also
#' note that the API was inspired by the `prioritizr` package.
#'
#' This package contains several vignettes to detail its usage and
#' showcase its features. You can explore these vignettes using
#' `browseVignettes("restoptr")`
#'
#' @references
#' Hanson JO, Schuster R, Morrell N, Strimas-Mackey M, Edwards BPM, Watts ME,
#' Arcese P, Bennett J, Possingham HP (2022). prioritizr: Systematic Conservation
#' Prioritization in R. R package version 7.1.1. Available at
#' https://CRAN.R-project.org/package=prioritizr.
#'
#' Jaeger, J. A. G. (2000). Landscape division, splitting index, and effective
#' mesh size: New measures of landscape fragmentation. Landscape Ecology, 15(2),
#' 115‑130.
#'
#' Justeau-Allaire, D., Vieilledent, G., Rinck, N., Vismara, P., Lorca, X.,
#' & Birnbaum, P. (2021). Constrained optimization of landscape indices in
#' conservation planning to support ecological restoration in New Caledonia.
#' Journal of Applied Ecology, 58(4), 744‑754.
#'
#' Pascual-Hortal, L., & Saura, S. (2006).
#' Comparison and development of new graph-based landscape connectivity indices:
#' Towards the priorization of habitat patches and corridors for conservation.
#' Landscape Ecology, 21(7), 959‑967.
#'
#' Prud'homme, C., Fages, J.-G., & Lorca, X. (2017). Choco documentation.
#'
#' @name restoptr
#'
#' @docType package
"_PACKAGE"

#' @import terra
#' @import rJava
#' @import units
NULL
