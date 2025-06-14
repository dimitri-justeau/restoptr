.onAttach <- function(libname, pkgname) {
}

.onLoad <- function(libname, pkgname) {

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
  jar_path <- file.path(libname, pkgname, "java", basename(restopt_jar_name))

  # initialize Java for package
  rJava::.jpackage(pkgname, lib.loc = libname)
  rJava::J("java.lang.System")$setProperty("EPSG-HSQL.directory", tempdir())
}

restopt_jar_name <- "restopt-2.1.0.jar"
