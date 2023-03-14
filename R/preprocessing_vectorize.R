#' @include internal.R
NULL


#' Restopr input vectorization-based preprocessing function.
#'
#' @export
preprocess_input_vect <- function(habitat_data, non_habitat_value, cellsize, area_unit = "ha") {
  names(habitat_data) <- "habitat"
  vectorized <- terra::as.polygons(habitat_data)
  vectorized <- vectorized[vectorized$habitat > non_habitat_value, ]
  vectorized_disagg <- terra::disagg(vectorized)
  grid <- terra::vect(sf::st_make_grid(sf::st_as_sf(vectorized_disagg), cellsize = cellsize, square = FALSE))
  grid <- terra::erase(grid, vectorized)
  grid$habitat <- non_habitat_value
  all <- terra::union(grid, vectorized_disagg)
  all$area <- terra::expanse(all, unit=area_unit)
}

