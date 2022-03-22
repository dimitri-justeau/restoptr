context("add_compactness_constraint")

test_that("add_compactness_constraint", {
  # Create problem
  habitat <- terra::rast(system.file("extdata", "habitat_hi_res.tif", package = "restoptr"))
  problem <- restopt_problem(habitat, aggregation_factor = 16, habitat_threshold = 0.7) %>%
    add_compactness_constraint(max_diameter = 6)
  result <- solve(problem)
  rest_cells <- which(result[,] == 3)
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
