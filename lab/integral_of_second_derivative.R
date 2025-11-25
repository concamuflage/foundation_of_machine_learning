# integral > 0, the slope has increased.


# Define f and derivatives
f  <- function(x) x^3
fp <- function(x) 3 * x^2
fpp <- function(x) 6 * x

# Interval
a <- 0
b <- 2

# Integral of second derivative
integral_result <- integrate(fpp, lower = a, upper = b)$value

# Slope change
slope_change <- fp(b) - fp(a)

# Print results
cat("Integral of f'' from 0 to 2:", integral_result, "\n")
cat("Slope change f'(2) - f'(0):", slope_change, "\n")

# ---- Plot ----
x_vals <- seq(0, 2, length.out = 200)

plot(x_vals, f(x_vals), type="l", col="blue", lwd=2,
     ylim=c(0, max(f(x_vals))),
     main="Function, First Derivative, and Second Derivative",
     xlab="x", ylab="value")

lines(x_vals, fp(x_vals), col="red", lwd=2)
lines(x_vals, fpp(x_vals), col="green", lwd=2)

legend("topleft",
       legend=c("f(x) = x^3", "f'(x) = 3x^2", "f''(x) = 6x"),
       col=c("blue", "red", "green"), lwd=2)

# -----------integral is 0, the slope hasn't changed --------

# ---- Define functions ----
f  <- function(x) x^3 - 3*x        # original function
fp <- function(x) 3*x^2 - 3         # first derivative
fpp <- function(x) 6*x              # second derivative

# ---- Compute integral of second derivative ----
result <- integrate(fpp, lower = -1, upper = 1)$value
cat("Integral of f'' from -1 to 1 =", result, "\n")

# ---- Create data for plotting ----
x_vals <- seq(-2, 2, length.out = 400)

# ---- Combined plot ----
plot(x_vals, f(x_vals), type="l", lwd=2, col="blue",
     ylim=c(min(fpp(x_vals), f(x_vals), fp(x_vals)),
            max(fpp(x_vals), f(x_vals), fp(x_vals))),
     main="Combined Plot: f(x), f'(x), f''(x)",
     xlab="x", ylab="Function value")

lines(x_vals, fp(x_vals),  col="red",    lwd=2)
lines(x_vals, fpp(x_vals), col="darkgreen", lwd=2)

abline(h=0, lty=2, col="gray50")
abline(v=0, lty=2, col="gray50")

legend("topleft",
       legend=c("f(x) = x^3 - 3x",
                "f'(x) = 3x^2 - 3",
                "f''(x) = 6x"),
       col=c("blue","red","darkgreen"),
       lwd=2)