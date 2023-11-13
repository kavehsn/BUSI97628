# Install latex2exp if you haven't already
# install.packages("latex2exp")

# Load the latex2exp package
library(latex2exp)

# AR(1) generator function
generate_ar1 <- function(phi, c, sigma, n) {
  # phi: AR(1) coefficient
  # c: constant term
  # sigma: standard deviation of the noise
  # n: length of the series to be generated
  
  # Pre-allocate the vector (for efficiency)
  ar1 <- numeric(n)
  
  # Set the first value of the series (can be zero or any other value)
  if(phi < 1 && phi > -1){
    ar1[1] <- rnorm(1, mean = c, sd = 1/sqrt((1-phi^2)))
  }else{
    ar1[1] <- rnorm(1, mean = c, sd = 1)
  }
  # Generate the AR(1) series
  for (t in 2:n) {
    ar1[t] <- c + phi * ar1[t-1] + rnorm(1, mean = 0, sd = sigma)
  }
  
  return(ar1)
}

# Example usage of the function:

setSeed = FALSE 

if(setSeed == TRUE){
  set.seed(123) # Set a seed for reproducibility
}

rhos <- c(0, 0.5, 0.99, 1)

png("ARplots.png") # Run this line only if you wish to save the plot

# Set up a 2x2 plot layout
par(mfrow = c(2, 2))

# Plot the series in a 2x2 layout
for (i in 1:length(phis)) {
  ar1_series_1 <- generate_ar1(phi = rhos[i], c = 0, sigma = 1, n = 1000)
  plot(ar1_series_1, type = 'l', xlab =  TeX("Time ($t$)"), ylab = TeX("$X_t$"), main = TeX(paste("$X_t, \\rho=", phis[i], "$", sep="")))
  grid()
}
dev.off() # Run this line only if you wish to save the plot

png("ACFplots.png") # Run this line only if you wish to save the plot

# Set up a 2x2 plot layout
par(mfrow = c(2, 2))

# Plot the series in a 2x2 layout
for (i in 1:length(phis)) {
  ar1_series_1 <- generate_ar1(phi = rhos[i], c = 0, sigma = 1, n = 1000)
  acf(ar1_series_1, main = TeX(paste("$X_t, \\rho=", phis[i], "$", sep="")))
  grid()
}
dev.off() # Run this line only if you wish to save the plot
