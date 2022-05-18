context("solve")

test_that("no solution due to error", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_restorable_constraint(
      min_restore = 400, max_restore = 400, unit = "ha", min_proportion = 1
    ) %>%
    add_compactness_constraint(1800, unit = "m")
  # tests
  expect_error(solve(problem))
})

test_that("no solution due to time limit", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_restorable_constraint(
      min_restore = 600, max_restore = 600, unit = "ha", min_proportion = 1
    ) %>%
    add_compactness_constraint(3.1, unit = "km") %>%
    add_components_constraint(10, 10) %>%
    add_settings(time_limit = 1)
  # tests
  expect_error(solve(problem))
})

test_that("optimal solution found", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_restorable_constraint(
      min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 1
    ) %>%
    add_compactness_constraint(4, unit = "cells") %>%
    add_components_constraint(1, 1) %>%
    set_max_mesh_objective()
  result <- solve(problem)
  md <- get_metadata(result)
  # tests
  expect_is(result, "RestoptSolution")
  expect_true(md$optimality_proven)
})

test_that("suboptimal solution found", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_restorable_constraint(
      min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 1
    ) %>%
    add_compactness_constraint(6, unit = "cells") %>%
    set_max_mesh_objective() %>%
    add_settings(time_limit = 1)
  result <- solve(problem)
  md <- get_metadata(result)
  # tests
  expect_is(result, "RestoptSolution")
  expect_false(md$optimality_proven)
})

test_that("desired number of solutions found", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_restorable_constraint(
      min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 1
    ) %>%
    add_compactness_constraint(6, unit = "cells") %>%
    add_settings(nb_solutions = 10)
  result <- solve(problem)
  # tests
  expect_is(result, "list")
  expect_length(result, 10)
  for (i in seq(1, 10)) {
    r <- result[[i]]
    testthat::expect_is(r, "RestoptSolution")
    testthat::expect_equal(names(r), paste("Solution", i))
  }
})

test_that("number of solutions found is less than desired", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_restorable_constraint(
      min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 1
    ) %>%
    add_compactness_constraint(6, unit = "cells") %>%
    set_max_mesh_objective() %>%
    add_settings(time_limit = 5, nb_solutions = 10)
  result <- solve(problem)
  # tests
  expect_is(result, "RestoptSolution")
})

test_that("only one optimal MESH solution exists, so it is returned", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_restorable_constraint(
      min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 1
    ) %>%
    add_compactness_constraint(4, unit = "cells") %>%
    set_max_mesh_objective() %>%
    add_settings(time_limit = 10, nb_solutions = 10)
  result <- solve(problem)
  # tests
  expect_is(result, "RestoptSolution")
})

test_that("multiple solutions found with IIC optimization", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_restorable_constraint(
      min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 1
    ) %>%
    add_compactness_constraint(4, unit = "cells") %>%
    set_max_iic_objective() %>%
    add_settings(time_limit = 10, nb_solutions = 10)
  result <- solve(problem)
  # tests
  expect_length(result, 10)
  for (i in seq(1, 10)) {
    r <- result[[i]]
    expect_true(inherits(r, "RestoptSolution"))
    expect_equal(names(r), paste("Solution", i))
    a <- get_metadata(r)
    expect_true(a$optimality_proven)
  }
})

test_that("number of solutions found is less than desired (no objective)", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_restorable_constraint(
      min_restore = 109, max_restore = 110, unit = "ha", min_proportion = 0.7
    ) %>%
    add_compactness_constraint(3, unit = "cells") %>%
    set_no_objective() %>%
    add_settings(time_limit = 2, nb_solutions = 1000)
  result <- solve(problem)
  # tests
  expect_is(result, "list")
  expect_is(result[[1]], "RestoptSolution")
  expect_true(length(result) < 1000)
})

test_that("multiple solutions found with IIC optimization and optimality gap", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
    ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_restorable_constraint(
      min_restore = 90, max_restore = 110, unit = "ha", min_proportion = 1
    ) %>%
    add_compactness_constraint(4, unit = "cells") %>%
    set_max_iic_objective() %>%
    add_settings(time_limit = 10, nb_solutions = 20, optimality_gap = 0.05)
  result <- solve(problem)
  # tests
  expect_length(result, 20)
  iic_best <- get_metadata(result[[1]])$iic_best
  for (i in seq(1, 20)) {
    r <- result[[i]]
    expect_true(inherits(r, "RestoptSolution"))
    expect_equal(names(r), paste("Solution", i))
    a <- get_metadata(r)
    expect_true(a$iic >= iic_best * 0.95)
  }
})
