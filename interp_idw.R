# ---------------------------------------------
# Inverse Distance Weighting - IDW
# ---------------------------------------------
#
# For each grid cell (or each query location), IDW calculates a weighted average of nearby sample points, 
# with weights inversely proportional to distance. The weight is computed as 'w = 1/distance'.

# x, y: coordinates
# z: temperature values
# Return data frame with x_grid, y_grid, z_pred

idw_interpol <- function(x, y, z, grid_size = 50) {
  
  # Create a grid for interpolation
  x_seq <- seq(min(x), max(x), length.out = grid_size)
  y_seq <- seq(min(y), max(y), length.out = grid_size)
  grid <- expand.grid(x = x_seq, y = y_seq)
  
  z_pred <- sapply(seq_len(nrow(grid)), function(i) {
    dist <- sqrt((grid$x[i] - x)^2 + (grid$y[i] - y)^2)  # distance to each measured point
    w <- 1 / (dist + 1e-6)  # weights
    sum(z * w) / sum(w)
  })

  grid$z_pred <- z_pred
  return(grid)
}
# ---------------------------------------------
