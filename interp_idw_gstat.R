# ---------------------------------------------
# Inverse Distance Weighting using gstat library - IDW
# ---------------------------------------------
#
# For each grid cell (or each query location), IDW calculates a weighted average of nearby sample points, 
# with weights inversely proportional to distance. The closer an observed point is to the prediction 
# location, the more influence (or weight) it has. More sophisticated then base IDW.

library(gstat)
library(sp)

# x, y: coordinates
# z: variable values (temperature, moisture)
# Return data frame with x_grid, y_grid, z_pred

idw_gstat_interpol <- function(x, y, z, grid_size=50) {
  # Convert to a Spatial object
  sp_data <- SpatialPointsDataFrame(
    coords = cbind(x, y),
    data = data.frame(z = z)
  )
  # Create a grid for newdata
  x_seq <- seq(min(x), max(x), length.out = grid_size)
  y_seq <- seq(min(y), max(y), length.out = grid_size)
  grid_df <- expand.grid(x = x_seq, y = y_seq)
  sp_grid <- SpatialPoints(grid_df)
  
  # IDW
  g <- gstat(id="z", formula = z~1, locations = sp_data, set = list(idp=2)) # idp=2 for IDW power parameter
  idw_result <- predict(g, sp_grid)
  
  # Combine result
  grid_df$z_pred <- idw_result$z.pred
  return(grid_df)
}
# ---------------------------------------------