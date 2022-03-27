
<!--- README.md is generated from README.Rmd. Please edit that file -->

## restopr: Interface to the ‘Restopt’ Ecological Restoration Planning Software

[![lifecycle](https://img.shields.io/badge/Lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check-Ubuntu](https://img.shields.io/github/workflow/status/dimitri-justeau/restoptr/Ubuntu/master.svg?label=Ubuntu)](https://github.com/dimitri-justeau/restoptr/actions)
[![R-CMD-check-Windows](https://img.shields.io/github/workflow/status/dimitri-justeau/restoptr/Windows/master.svg?label=Windows)](https://github.com/dimitri-justeau/restoptr/actions)
[![R-CMD-check-MacOS](https://img.shields.io/github/workflow/status/dimitri-justeau/restoptr/macOS/master.svg?label=macOS)](https://github.com/dimitri-justeau/restoptr/actions)
[![Coverage
Status](https://codecov.io/github/dimitri-justeau/restoptr/coverage.svg?branch=master)](https://app.codecov.io/gh/dimitri-justeau/restoptr)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/restoptr)](https://github.com/dimitri-justeau/restoptr)

-   [Overview](#overview)
-   [Installation](#installation)
    -   [Package installation](#package_install)
    -   [System dependencies](#system_deps)
-   [Citation](#citation)
-   [Usage](#usage)
-   [Getting help](#help)

## Overview <a name="overview"></a>

Interface to the `restopt` ecological restoration planning software,
which is specifically designed to identify connected, accessible, and
compact areas for ecological restoration, with an emphasis on reducing
habitat fragmentation and increasing habitat connectivity. This software
reproduces the methodology described in [this
article](https://www.researchgate.net/publication/346597935_Constrained_optimization_of_landscape_indices_in_conservation_planning_to_support_ecological_restoration_in_New_Caledonia),
and is based on Constraint Programming (CP), which is a constrained
optimization solving technique based on automated reasoning.
Specifically, our model was implemented using
[Choco-solver](https://choco-solver.org/), an open-source and
state-of-the-art Java CP solver.

## Installation <a name="installation"></a>

### Package installation <a name="package_install"></a>

The latest developmental version of the *restoptr R* package can be
installed using the following *R* code.

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("dimitri-justeau/restoptr")
```

Or with *devtools*:

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("dimitri-justeau/rflsgen")
```

### System dependencies <a name="system_deps"></a>

The packages requires a Java Runtime Environment (JRE), version 8 or
higher. Below we provide platform-specific instructions to install it.

#### *Windows*

Please install the latest Java Development Kit from
[Oracle](www.oracle.com) website. To achieve this, navigate to the
[downloads section of the
website](https://www.oracle.com/java/technologies/javase-downloads.html),
select the tab for the Windows operating system, and then download the
x64 Installer file. After downloading the file, please run installer to
install Java on your system. You will also need to ensure that the
`PATH` environmental variable if configured so that *R* can access Java.
*restoptr* relies on *rJava* for the communication between *R* and
*Java*. If you have any trouble during the installation of *restopt* due
to *rJava*, please refer to *rJava*’s documentation:
<https://rforge.net/rJava/index.html>.

#### *Ubuntu*

For recent versions of Ubuntu (18.04 and later), the Java libraries are
available through official repositories. They can be installed using the
following system commands.

``` bash
sudo apt-get install default-jdk
```

If you want to install a specific JRE version, please follow
instructions from
[Oracle](https://www.oracle.com/java/technologies/javase-downloads.html),
or [OpenJDK](https://openjdk.java.net/install/).

#### *Linux*

Please follow instructions from
[Oracle](https://www.oracle.com/java/technologies/javase-downloads.html),
or [OpenJDK](https://openjdk.java.net/install/).

#### *MacOS*

The easiest way to install the Java libraries is using
[HomeBrew](https://brew.sh/). After installing HomeBrew, the Java
libraries can be installed using the following system commands.

``` bash
brew install openjdk
```

Please note that you might also need to ensure that the `PATH`
environmental variable if configured so that *R* can access Java.

## Citation <a name="citation"></a>

Please cite *restoptr* when using it in publications.

> Justeau-Allaire, D., Vieilledent, G., Rinck, N., Vismara, P., Lorca,
> X., & Birnbaum, P. (2021). Constrained optimization of landscape
> indices in conservation planning to support ecological restoration in
> New Caledonia. Journal of Applied Ecology, 58(4), 744‑754.

This article describes the methodology. We will provide a specific
citation for ressource the R package soon.

## Usage <a name="usage"></a>

The first thing to do to use *restoptr* is to load the package:

``` r
library(restoptr)
```

We will now create and solve a restoration optimization problem. One
input raster are necessary, the *existing_habitat* raster, which is a
binary raster indicating the habitat areas (raster value 1), the
non-habitat areas (raster value 0), and areas that are not part of the
landscape (raster value NA, or NODATA, e.g. ocean if the landscape is
terrestrial). Note that the resolution can be high, as the down sampling
of the input data to ensure a tractable optimization problem is part of
the restoration optimization problem construction (see the next
paragraph).

Along with this *existing_habitat* input raster, it is necessary to
provide two parameters that inform `restoptr` how to eventually reduce
the resolution to ensure a tractable optimization problem: the
*aggregation_factor* and the *habitat_threshold* :

-   *aggregation_factor*: if the resolution of the *existing_habitat* is
    high, it can be unsuited to use as is into the constrained
    optimization engine, which will either be limited by the system’s
    available RAM, or have a very long computation time. The
    *aggregation_factor* indicates how to reduce the resolution of the
    aggregating cells (see `terra::aggregate()`). The amount of habitat
    and restorable area within each aggregated cell will be
    automatically computed.

-   *habitat_threshold*: as the lower resolution habitat raster that
    will be used into the constrained optimization engine must be
    binary, it is necessary to define a condition which indicates
    whether an aggregated is considered as habitat or not. This
    condition is defined by the *habitat_threshold*, which indicates how
    much proportion of habitat is needed into an aggregated cell to
    consider it as habitat.

**Note:** In the following, we will also refer to the aggregated cells
as **planning units**.

**Note:** we implemented this data pre processing step as a new feature
in `restoptr` to facilitate its usage, to ensure that problems are
instantiated with consistent information, and to facilitate the
automation of workflows.

Example data, from the use case presented in [this
study](https://www.researchgate.net/publication/346597935_Constrained_optimization_of_landscape_indices_in_conservation_planning_to_support_ecological_restoration_in_New_Caledonia)
is included in the package:

``` r
habitat_data <- rast(
  system.file("extdata", "habitat_hi_res.tif", package = "restoptr")
)
plot(habitat_data, plg=list(x="topright"))
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" style="display: block; margin: auto;" />

To instantiate a base restoration optimization problem from such an
input raster, use the `restopt_problem()` function:

``` r
p <- restopt_problem(
  existing_habitat = habitat_data, 
  aggregation_factor = 16,
  habitat_threshold = 0.7
) 
```

Input data and preprocessed aggregated rasters can be accessed with:

-   `get_original_habiata(p)` is the input, high resolution habitat
    raster.
-   `get_existing_habitat(p)` is the aggregated habitat raster.
-   `get_restorable_habitat(p)` is the aggregated restorable area
    raster.
-   `get_cell_area(p)` is the area of each planning, in number of cells
    from the original raster. Note that this area is not necessarily the
    same for all planning units, as if there are NA cells in the input
    raster some planning units can partially cover NA and non-NA cells.

We can plot the aggregated data:

``` r
plot(rast(list(p$data$existing_habitat, p$data$restorable_habitat)), nc = 2,  plg=list(x="topright"))
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" style="display: block; margin: auto;" />

Then, we can add constraints to this base problem. For instance, lets
add a locked-out constraint, to restrict the number of planning units
that can be selected for restoration. Such a constraint can be used to
account for existing land-use practices, feasibility of restoration
activities, and stakeholder preferences.

``` r
locked_out_data <- rast(
 system.file("extdata", "locked_out.tif", package = "restoptr")
)
p <- p %>% add_locked_out_constraint(data = locked_out_data)
```

We can also add a constraint on the amount of restored area that is
allowed by the project:

``` r
p <- p %>% add_restorable_constraint(90, 220, unit = "ha")
```

And finally a compactness constraint, which limits the spatial extent of
the selected restoration area:

``` r
p <- p %>% add_compactness_constraint(2.4, unit = "km")
```

Once we have added constraints to the problem, we need to define an
optimization objective. For example, lets configure `restopr` to
identify, under the previous constraints, which restoration areas
maximizes the effective mesh size (MESH).

``` r
p <- p %>% set_max_mesh_objective()
```

*Note* The effective mesh size is a measure of landscape fragmentation
based on the probability that two randomly chosen points are located in
the same patch [Jaeger, 2000](https://doi.org/10.1023/A:1008129329289).
Maximizing it in the context of restoration favours fewer and larger
patches.

We can get a summary of the restoration problem:

``` r
p
```

    ## -----------------------------------------------------------------
    ##                          Restopt problem                         
    ## -----------------------------------------------------------------
    ## original habitat:     habitat_hi_res.tif 
    ## aggregation factor:   16 
    ## habitat threshold:    0.7 
    ## existing habitat:     in memory 
    ## restorable habitat:   in memory 
    ## ----------------------------------------------------------------- 
    ## objective:            Maximize effective mesh size 
    ## ----------------------------------------------------------------- 
    ## constraints:          
    ##   -  locked out (data = in memory) 
    ##   -  restorable (min_restore = 90, max_restore = 220, unit = ha) 
    ##   -  compactness (max_diameter = 2.4, unit = km) 
    ## ----------------------------------------------------------------- 
    ## settings:
    ##   - precision = 4
    ##   - time_limit = 0
    ## -----------------------------------------------------------------

Finally, we use the `solve()` function to identify the optimal
restoration area, according to the constraints and the optimization
objective.

``` r
s <- solve(p)
```

    ## Good news: the solver found a solution statisfying the constraints that was proven optimal ! (solving time = 1.3754321 s)

``` r
plot(
  s,
  main = "Solution",
  col = c("#E5E5E5", "#fff1d6", "#b2df8a", "#1f78b4"),
  plg = list(x="topright")
)
```

<img src="man/figures/README-unnamed-chunk-16-1.png" width="100%" style="display: block; margin: auto;" />

You can retrieve the attributes of the solution using the
`get_metadata()` function:

``` r
get_metadata(s, area_unit = "ha")
```

    ##     min_restore total_restorable nb_planning_units optimality_proven
    ## 1 219.3772 [ha]    219.3772 [ha]                15              true
    ##   solving_time  mesh_initial     mesh_best
    ## 1     1.375432 53.38999 [ha] 55.59634 [ha]

## Getting help <a name="help"></a>

If you have any questions about *restoptr*, improvement suggestions, or
if you detect a bug, please [open an
issue](https://github.com/dimitri-justeau/restoptr/issues/new/choose) in
this GitHub repository.
