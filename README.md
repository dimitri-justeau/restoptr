<a name="down"></a>
## Download and installation

Java 8+ must be installed in your system to run restoptr. Download and installation instructions for Java are available here: https://www.oracle.com/java/technologies/javase-downloads.html, or here: https://openjdk.java.net/install/. To provide an R interface to restoptr, which is written in Java, restoptr relies on rJava, which is a dependency of restoptr and will be installed automatically. If you have any trouble during the installation of restoptr due to rJava, please refer to rJava's documentation: https://rforge.net/rJava/index.html.

To install restoptr from Github, you can use the devtools library (https://www.r-project.org/nosvn/pandoc/devtools.html) and use the following commands in R:

```r
devtools::install_github("dimitri-justeau/restoptr")
library(restoptr)
```

<a name="tuto"></a>
## Tutorial

To reproduce the example from https://github.com/dimitri-justeau/restopt

### Example : maximize MESH

```
  library(restoptr)
  habitat <- raster("./example_data/habitat.tif")
  restorable <- raster("./example_data/restorable.tif")
  accessible <- raster("./example_data/accessible.tif")
  problem <- RestoptProblem(habitat=habitat, restorable=restorable, accessible=accessible)
  postNbComponentsConstraint(problem, 1, 1)
  postRestorableConstraint(problem, 90, 110, 23, 0.7)
  postCompactnessConstraint(problem, 6)
  result <- maximizeMESH(problem, 3)
```

### Example : maximize IIC

```
  library(restoptr)
  habitat <- raster("./example_data/habitat.tif")
  restorable <- raster("./example_data/restorable.tif")
  accessible <- raster("./example_data/accessible.tif")
  problem <- RestoptProblem(habitat=habitat, restorable=restorable, accessible=accessible)
  postNbComponentsConstraint(problem, 1, 1)
  postRestorableConstraint(problem, 90, 110, 23, 0.7)
  postCompactnessConstraint(problem, 6)
  result <- maximizeIIC(problem, 3)
```
