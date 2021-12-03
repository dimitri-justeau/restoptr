context("restopt_problem")

test_that("create_problem", {
  habitat <- terra::rast(system.file("extdata", "habitat.tif", package = "restoptr"))
  restorable <- terra::rast(system.file("extdata", "restorable.tif", package = "restoptr"))
  accessible <- terra::rast(system.file("extdata", "accessible.tif", package = "restoptr"))
  problem <- restopt_problem(habitat, restorable) %>%
    add_locked_out_constraint(accessible) %>%
    add_components_constraint(min_nb_components = 1, max_nb_components = 1) %>%
    add_compactness_constraint(max_diameter = 6) %>%
    add_restorable_constraint(min_restore = 90, max_restore = 110, cell_area = 23, min_proportion = 0.7)

  testthat::expect_equal(class(problem), "RestoptProblem")


  # problem <- RestoptProblem(habitat=habitat, restorable=restorable, accessible=accessible)
  # testthat::expect_equal(problem@habitat_path, terra::sources(habitat)[[1]])
  # testthat::expect_equal(problem@restorable_path, terra::sources(restorable)[[1]])
  # testthat::expect_equal(problem@accessible_path, terra::sources(accessible)[[1]])
  # testthat::expect_false(hasNbComponentsConstraint(problem))
  # testthat::expect_false(hasCompactnessConstraint(problem))
  # testthat::expect_false(hasRestorableConstraint(problem))
  # problem <- postNbComponentsConstraint(problem, 1, 1)
  # testthat::expect_true(hasNbComponentsConstraint(problem))
  # problem <- postRestorableConstraint(problem, 90, 110, 23, 0.7)
  # testthat::expect_true(hasRestorableConstraint(problem))
  # problem <- postCompactnessConstraint(problem, 6)
  # testthat::expect_true(hasCompactnessConstraint(problem))
})
