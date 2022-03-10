context("restopt_problem")

test_that("add_compactness_constraint", {
  # Create problem
  habitat <- terra::rast(system.file("extdata", "habitat.tif", package = "restoptr"))
  restorable <- terra::rast(system.file("extdata", "restorable.tif", package = "restoptr"))
  problem <- restopt_problem(habitat, restorable) %>%
    add_compactness_constraint(max_diameter = 6)
  result <- solve(problem)
  rest_cells <- which(result[,] == 2)
  for (i in 1:length(rest_cells)) {
    for (j in i:length(rest_cells)) {
      if (i != j) {
        ci = rowColFromCell(result, rest_cells[[i]])
        cj = rowColFromCell(result, rest_cells[[j]])
        d = sqrt((cj[[1]] - ci[[1]])^2 + (cj[[2]] - ci[[2]])^2)
        testthat::expect_lte(d, 6)
      }
    }
  }
})
