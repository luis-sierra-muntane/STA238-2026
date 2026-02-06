# 1. Setup
set.seed(238)      # For reproducibility
n_sims <- 10000     # Number of experiments
n <- 50             # Sample size per experiment
lambda <- 2         # True lambda
true_p0 <- exp(-lambda) # True Probability P(X=0) approx 0.1353

# Vectors to store our estimates
est_indicator <- numeric(n_sims)
est_mle <- numeric(n_sims)

# Simulation
for(i in 1:n_sims) {
  x <- rpois(n, lambda)
  
  # Estimator 1: The Indicator (Count zeros / n)
  est_indicator[i] <- mean(x == 0)
  
  # Estimator 2: The MLE (e^-mean)
  est_mle[i] <- exp(-mean(x))
}

calc_mse <- function(estimates, truth) { mean((estimates - truth)^2) }

# Print comparisons
cat("True P(X=0):", round(true_p0, 5), "\n\n")

cat("--- Indicator Estimator ---\n")
cat("Mean Estimate:", round(mean(est_indicator), 5), "\n")
cat("Variance:     ", round(var(est_indicator), 5), "\n")
cat("MSE:          ", round(calc_mse(est_indicator, true_p0), 5), "\n\n")

cat("--- MLE Estimator ---\n")
cat("Mean Estimate:", round(mean(est_mle), 5), "\n")
cat("Variance:     ", round(var(est_mle), 5), "\n")
cat("MSE:          ", round(calc_mse(est_mle, true_p0), 5), "\n")

cat("--- Efficiency ---\n")
cat("Ratio of Variances (MLE / Indicator):", 
    round(var(est_mle) / var(est_indicator), 3))
# Visualizing the difference
par(mfrow=c(2,1))
xlims <- c(0, 0.4) # Fixed range for easy comparison

hist(est_indicator, breaks=30, xlim=xlims, col=rgb(1,0,0,0.5), 
     main="Indicator Estimator (Unbiased)", xlab="Estimate of P(X = 0)")
abline(v=true_p0, col="black", lwd=2, lty=2)

hist(est_mle, breaks=30, xlim=xlims, col=rgb(0,0,1,0.5), 
     main=expression(paste("Estimator ", e^{bar(X)[n]}, " (Biased)")),
     xlab="Estimate of P(X = 0)")
abline(v=true_p0, col="black", lwd=2, lty=2)

