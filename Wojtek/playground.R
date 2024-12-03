# Define the cumulative distribution function
F_x <- function(x) {
  if (x < 1) {
    return(0)
  } else {
    n <- floor(x)
    return(1 - 1/n)
  }
}

# Generate a sequence of x values to plot
x_values <- seq(0, 10, by = 0.1)
y_values <- sapply(x_values, F_x)

# Plot the cumulative distribution function
plot(x_values, y_values, type = "s")






# Define the interval for the uniform distribution
a <- 0
b <- 4

# Expected value E(2X + 1)
E_2X_plus_1 <- function(x) {
  (2 * x + 1) * (1 / (b - a))
}
integrate(E_2X_plus_1, lower = a, upper = b)$value

# Expected value E(X^2 + 5)
E_X2_plus_5 <- function(x) {
  (x^2 + 5) * (1 / (b - a))
}
integrate(E_X2_plus_5, lower = a, upper = b)$value







# Define the PDF function
f_x <- function(x) {
  if (x <= 0) {
    return(0)
  } else if (x > 0 && x <= 1) {
    return(1/2)
  } else {
    return(1 / (2 * x^2))
  }
}

# Generate a sequence of x values to plot the PDF
x_values <- seq(-0.5, 3, by = 0.01)
y_values <- sapply(x_values, f_x)

# Plot the PDF
plot(x_values, y_values, type = "l")
