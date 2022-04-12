.onLoad <- function(libname, pkgname) {
  # set Java parameters if none specified
  if (is.null(getOption("java.parameters"))) {
    options(java.parameters = "-Xmx4g")
  }

  # initialize java
  rJava::.jinit()

  # check Java version is acceptable
  jv <- rJava::.jcall(
    "java/lang/System", "S", "getProperty", "java.runtime.version"
  )
  if (substr(jv, 1L, 2L) == "1.") {
    jvn <- as.numeric(paste0(strsplit(jv, "[.]")[[1L]][1:2], collapse = "."))
    if (jvn < 1.8) {
      stop("Java (version 8+) is not available for the restoptr package")
    }
  }

  # specify file path for restopt jar file
  jar_path <- file.path(libname, pkgname, "java", basename(restopt_url))

  # download restopt jar file if needed
  # see https://github.com/dimitri-justeau/restopt
  if (!file.exists(jar_path)) {
    ## create directory to store jar file if needed
    if (!file.exists(dirname(jar_path))) {
      dir.create(dirname(jar_path), showWarnings = FALSE, recursive = TRUE)
    }
    ## print message to explain why files are being downloaded
    packageStartupMessage(
      "Downloading Java software to generate prioritizations..."
    )
    ## download jar file
    old_options <- options(timeout = max(1000, getOption("timeout")))
    on.exit(options(old_options))
    utils::download.file(restopt_url, destfile = jar_path, mode = "wb")
  }

  # initialize Java for package
  rJava::.jpackage(pkgname, lib.loc = libname)
  rJava::J("java.lang.System")$setProperty("EPSG-HSQL.directory", tempdir())
}

# define donwload URL for restopt jar file
restopt_url <- "https://github.com/dimitri-justeau/restopt/releases/download/2.0.0/restopt-2.0.0.jar"
