context("restopt_problem")

test_that("create_problem", {

  ## Test valid inputs
  habitat <- terra::rast(system.file("extdata", "habitat.tif", package = "restoptr"))
  restorable <- terra::rast(system.file("extdata", "restorable.tif", package = "restoptr"))
  accessible <- terra::rast(system.file("extdata", "accessible.tif", package = "restoptr"))
  problem <- restopt_problem(habitat, restorable) %>%
    add_locked_out_constraint(accessible) %>%
    add_components_constraint(min_nb_components = 1, max_nb_components = 1) %>%
    add_compactness_constraint(max_diameter = 6) %>%
    add_restorable_constraint(min_restore = 90, max_restore = 110, cell_area = 23, min_proportion = 0.7) %>%
    add_max_mesh_objective()

  problem <- add_settings(problem, time_limit = 30)

  testthat::expect_equal(class(problem), "RestoptProblem")
  testthat::expect_equal(length(problem$constraints), 4)
  testthat::expect_true(inherits(problem$objective, "MaxMeshObjective"))
  testthat::expect_equal(problem$settings$time_limit, 30)

  ## Test invalid inputs
  testthat::expect_error(restopt_problem(1, restorable))
  testthat::expect_error(restopt_problem(habitat, 1))
  resized_habitat <- aggregate(habitat, 4)
  testthat::expect_error(restopt_problem(resized_habitat, restorable))
  multi_layer <- c(habitat, restorable)
  testthat::expect_error(restopt_problem(multi_layer, restorable))
  testthat::expect_error(restopt_problem(habitat, multi_layer))
  testthat::expect_error(restopt_problem(restorable, restorable))
  testthat::expect_error(restopt_problem(-habitat, restorable))
  testthat::expect_error(restopt_problem(habitat, -restorable))
})
