# ---------------------------------------------
# Akima linear interpolation
# ---------------------------------------------

# It triangulates the data domain and linearly interpolates within each triangle, producing 
# a continuous surface.

library(akima)

# x, y: coordinates
# z: temperature values
# Return data frame with x_grid, y_grid, z_pred

akima_interpol <- function(x, y, z, grid_size = 50) {
  
  # Create a regular grid for x and y
  x_seq <- seq(min(x), max(x), length.out = grid_size)
  y_seq <- seq(min(y), max(y), length.out = grid_size)
  
  # Make a 2D linear interpolation
  interp_res <- akima::interp(
    x = x,
    y = y,
    z = z,
    xo = x_seq,
    yo = y_seq,
    linear = TRUE,  # linear interpolation
    duplicate = "mean" # how to handle duplicate points
  )

  # Convert the akima output to a data frame
  grid_df <- expand.grid(x = interp_res$x, y = interp_res$y)
  grid_df$z_pred <- as.vector(interp_res$z)
  
  return(grid_df)
}
# ---------------------------------------------
