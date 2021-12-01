
<!--- README.md is generated from README.Rmd. Please edit that file -->

## restopr: Interface to the ‘Restopt’ Ecological Restoration Planning Software

[![lifecycle](https://img.shields.io/badge/Lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check-Ubuntu](https://img.shields.io/github/workflow/status/dimitri-justeau/restoptr/Ubuntu/master.svg?label=Ubuntu)](https://github.com/dimitri-justeau/restoptr/actions)
[![R-CMD-check-Windows](https://img.shields.io/github/workflow/status/dimitri-justeau/restoptr/Windows/master.svg?label=Windows)](https://github.com/dimitri-justeau/restoptr/actions)
[![R-CMD-check-MacOS](https://img.shields.io/github/workflow/status/dimitri-justeau/restoptr/Mac%20OSX/master.svg?label=MacOS)](https://github.com/dimitri-justeau/restoptr/actions)
[![Coverage
Status](https://codecov.io/github/dimitri-justeau/restoptr/coverage.svg?branch=master)](https://codecov.io/github/dimitri-justeau/restoptr?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/restoptr)](https://CRAN.R-project.org/package=restoptr)

## Overview

Interface to the `restopt` ecological restoration planning software,
which is specifically designed to identify connected, accessible, and
compact areas for ecological restoration, with an emphasis on reducing
habitat fragmentation and increasing habitat connectivity.

## Installation

### Package installation

The latest developmental version of the *restoptr R* package can be
installed using the following *R* code.

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("dimitri-justeau/restoptr")
```

### System dependencies

The packages requires [Java (version 8 or
higher)](https://www.java.com/). Below we provide platform-specific
instructions to install it.

#### *Windows*

Please install the latest Java Development Kit from
[Oracle](www.oracle.com) website. To achieve this, navigate to the
[downloads section of the
website](https://www.oracle.com/java/technologies/javase-downloads.html),
select the tab for the Windows operating system, and then download the
x64 Installer file. After downloading the file, please run installer to
install Java on your system. You will also need to ensure that the
`PATH` environmental variable if configured so that *R* can access Java.

#### *Ubuntu*

For recent versions of Ubuntu (18.04 and later), the Java libraries are
available through official repositories. They can be installed using the
following system commands.

``` bash
sudo apt-get install default-jdk
```

#### *Linux*

For Unix-alikes, `javac` (&gt;= 8) is required.

#### *MacOS*

The easiest way to install the Java libraries is using
[HomeBrew](https://brew.sh/). After installing HomeBrew, the Java
libraries can be installed using the following system commands.

``` bash
brew install openjdk
```

Please note that you might also need to ensure that the `PATH`
environmental variable if configured so that *R* can access Java.

## Citation

TODO.

## Usage

To reproduce the example from
<https://github.com/dimitri-justeau/restopt>

### Example : maximize MESH

``` r
library(restoptr)
habitat <- raster("./inst/extdata/habitat.tif")
restorable <- raster("./inst/extdata/restorable.tif")
accessible <- raster("./inst/extdata/accessible.tif")
problem <- RestoptProblem(habitat=habitat, restorable=restorable, accessible=accessible)
postNbComponentsConstraint(problem, 1, 1)
postRestorableConstraint(problem, 90, 110, 23, 0.7)
postCompactnessConstraint(problem, 6)
result <- maximizeMESH(problem, 3)
```

### Example : maximize IIC

``` r
library(restoptr)
habitat <- raster("./inst/extdata/habitat.tif")
restorable <- raster("./inst/extdata/restorable.tif")
accessible <- raster("./inst/extdata/accessible.tif")
problem <- RestoptProblem(habitat=habitat, restorable=restorable, accessible=accessible)
postNbComponentsConstraint(problem, 1, 1)
postRestorableConstraint(problem, 90, 110, 23, 0.7)
postCompactnessConstraint(problem, 6)
result <- maximizeIIC(problem, 3)
```

## Getting help

TODO.
