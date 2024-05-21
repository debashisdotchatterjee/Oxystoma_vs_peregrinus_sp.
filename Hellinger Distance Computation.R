# Simulate two posterior samples (replace with your actual MCMC samples)
set.seed(123)
posterior1 <-  ott10000.129[-(1:1000)]
posterior2 <-  ptt10000.129[-(1:1000)]

# Function to calculate Hellinger distance (discrete approximation)
# Function to calculate Hellinger distance (discrete approximation)
hellinger_distance <- function(x, y) {
  # Calculate density estimates (adjusting 'n' to get more points for interpolation)
  dens_x <- density(x, n = 512)
  dens_y <- density(y, n = 512)
 
  # Interpolate densities to match x-values for easier comparison
  dens_y_interp <- approxfun(dens_y$x, dens_y$y, rule = 2)
 
  # Ensure density values are non-negative
  dens_x$y <- pmax(dens_x$y, 0)
  dens_y$y <- pmax(dens_y$y, 0)
 
  # Create a common grid of x-values for integration
  # (using the range covered by both densities)
  common_x <- seq(max(min(dens_x$x), min(dens_y$x)), min(max(dens_x$x), max(dens_y$x)), length.out = 512)
 
  # Calculate interpolated densities on the common grid
  common_dens_x <- dens_x$y[match(common_x, dens_x$x)]
  common_dens_y <- dens_y_interp(common_x)
 
  # Replace NAs (if any) with 0 before calculating the distance
  common_dens_x[is.na(common_dens_x)] <- 0
  common_dens_y[is.na(common_dens_y)] <- 0
 
  # Calculate Hellinger distance (discrete approximation)
  sqrt(0.5 * sum((sqrt(common_dens_x) - sqrt(common_dens_y))^2) * mean(diff(common_x)))
}


# Calculate Hellinger distance
hdist <- hellinger_distance(posterior1, posterior2)

# Print the result
cat("Hellinger Distance (Discrete Approximation):", hdist, "\n")

