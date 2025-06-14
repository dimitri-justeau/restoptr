context("restopt_problem")

test_that("expect results", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  accessible_data <- terra::vect(system.file(
    "extdata", "accessible_areas.gpkg", package = "restoptr"
  ))
  # prepare locked out data
  locked_out_data <- invert_vector(
    accessible_data,
    extent = terra::ext(habitat_data),
    filter = accessible_data$ID == 1
  )
  # build problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_locked_out_constraint(locked_out_data) %>%
    add_components_constraint(min_nb_components = 1, max_nb_components = 1) %>%
    add_compactness_constraint(max_diameter = 6, unit = "cells") %>%
    add_restorable_constraint(
      min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 0.7
    ) %>%
    set_max_mesh_objective() %>%
    add_settings(time_limit = 30, nb_solutions = 5)
  # tests
  expect_output(print(problem))
  expect_equal(class(problem), "RestoptProblem")
  expect_equal(length(get_constraints(problem)), 4)
  expect_true(inherits(get_objective(problem), "MaxMeshObjective"))
  expect_equal(get_settings(problem)$time_limit, 30)
  expect_equal(get_aggregation_method(problem), "lossy")
  expect_equal(get_aggregation_factor(problem), 16)
  expect_equal(get_habitat_threshold(problem), 0.7)
  expect_true(inherits(get_original_habitat(problem), "SpatRaster"))
  expect_true(inherits(get_existing_habitat(problem), "SpatRaster"))
  expect_true(inherits(get_restorable_habitat(problem), "SpatRaster"))
  expect_true(inherits(get_locked_out_areas(problem), "SpatRaster"))
  expect_true(inherits(get_cell_area(problem), "SpatRaster"))
  expect_equal(
    max(as.vector(get_cell_area(problem)), na.rm = TRUE),
    get_aggregation_factor(problem)^2
  )
})

test_that("expected warnings when overwriting constraints", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_components_constraint(min_nb_components = 1, max_nb_components = 1)
  # tests
  expect_warning(
    problem <-
      problem %>%
      add_components_constraint(min_nb_components = 5, max_nb_components = 5)
  )
  expect_length(problem$constraints, 1)
  expect_equal(
    problem$constraints[[1]]$name,
    "components (min_nb_components = 5, max_nb_components = 5)"
  )
})

test_that("expected warnings when overwriting objective", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    set_max_mesh_objective()
  # tests
  expect_warning(
    problem <-
      problem %>%
      set_max_iic_objective()
  )
  expect_is(problem$objective, "MaxIicObjective")
})

test_that("invalid inputs", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  resized_habitat_data <- aggregate(habitat_data, 4)
  multi_layer_data <- c(habitat_data, habitat_data)
  # tests
  expect_error(restopt_problem(1))
  expect_error(restopt_problem(multi_layer_data, aggregation_factor = 16))
  expect_error(restopt_problem(habitat_data, aggregation_factor = -1))
  expect_error(
    restopt_problem(
      habitat_data, aggregation_factor = 2, habitat_threshold = 1.7
    )
  )
})

test_that("aggregation_factor_1", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
    ))
  accessible_data <- terra::vect(system.file(
    "extdata", "accessible_areas.gpkg", package = "restoptr"
  ))
  # prepare locked out data
  locked_out_data <- invert_vector(
    accessible_data,
    extent = terra::ext(habitat_data),
    filter = accessible_data$ID == 1
  )
  # build problem
  expect_warning(
    restopt_problem(habitat_data, habitat_threshold = 0.7, aggregation_factor = 1)
  )
  expect_warning(
    problem <-
      restopt_problem(habitat_data, 1, 1) %>%
      add_restorable_constraint(
        min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 0.7
      )
  )
  problem <-
    restopt_problem(habitat_data, 1, 1) %>%
    add_locked_out_constraint(locked_out_data) %>%
    add_components_constraint(min_nb_components = 1, max_nb_components = 1) %>%
    add_compactness_constraint(max_diameter = 6, unit = "cells") %>%
    add_restorable_constraint(
      min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 1
    ) %>%
    set_max_mesh_objective() %>%
    add_settings(time_limit = 30, nb_solutions = 5)

    # test
    or <- get_original_habitat(problem)
    ag <- get_existing_habitat(problem)
    expect_equal(nrow(or), nrow(ag))
    expect_equal(ncol(or), ncol(ag))
    expect_equal(minmax(or == ag)[[1]], 1)
    expect_equal(minmax(or == ag)[[2]], 1)
    re <- get_restorable_habitat(problem)
    expect_equal(length(which(re[which(or[,] == 1)] == 1)), 0)
})
