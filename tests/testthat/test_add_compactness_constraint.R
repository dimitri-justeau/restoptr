context("add_compactness_constraint")

test_that("expected results", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # build and solve problem
  problem <-
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_compactness_constraint(max_diameter = 6, unit = "cells") %>%
    add_settings(time_limit = 30)
  result <- solve(problem, verbose = TRUE)
  # tests
  expect_is(result, "SpatRaster")
  expect_gte(terra::global(result == 3, "sum", na.rm = TRUE), 1)
  rest_cells <- which(result[,] == 3)
  for (i in seq_along(rest_cells)) {
    for (j in seq_along(rest_cells)) {
      if (i != j) {
        ci = rowColFromCell(result, rest_cells[[i]])
        cj = rowColFromCell(result, rest_cells[[j]])
        d = sqrt((cj[[1]] - ci[[1]])^2 + (cj[[2]] - ci[[2]])^2)
        expect_lte(d, 6)
      }
    }
  }
})

test_that("invalid inputs", {
  # import data
  habitat_data <- terra::rast(
    system.file("extdata", "habitat_hi_res.tif", package = "restoptr"
  ))
  # tests
  expect_error(
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_compactness_constraint(max_diameter = -1, unit = "cells")
  )
  expect_error(
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_compactness_constraint(max_diameter = NA_real_, unit = "cells")
  )
  expect_error(
    restopt_problem(habitat_data, 0.7, 16) %>%
    add_compactness_constraint(max_diameter = 6, unit = "kg")
  )
})
