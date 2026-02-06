library(ggplot2)
library(gridExtra)

# Reproducibility
set.seed(238)
n_sims <- 10000
sample_sizes <- c(2, 10, 50)

# 1. Comparison of Means vs Sums
# This function generates data for both mean and sum
generate_clt_data <- function(dist_type, sizes) {
  df_list <- list()
  for (n in sizes) {
    samples <- replicate(n_sims, {
      if(dist_type == "exp") rexp(n, rate = 1)
      else runif(n, 0, 1)
    })
    
    # Calculate means and sums
    means <- colMeans(samples)
    sums <- colSums(samples)
    
    df_list[[as.character(n)]] <- data.frame(
      val = c(means, sums),
      n = as.factor(n),
      stat = rep(c("Mean", "Sum"), each = n_sims)
    )
  }
  do.call(rbind, df_list)
}

# Plot Exponential Means vs Sums
exp_data <- generate_clt_data("exp", sample_sizes)
p_exp <- ggplot(exp_data, aes(x = val, fill = n)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~stat, scales = "free") +
  labs(title = "CLT: Exponential Means vs. Sums", x = "Value", y = "Density") +
  theme_minimal()

# We use the fact that a Binomial(n, p) is the sum of n Bernoulli trials
binom_clt_plot <- function(n, p = 0.2) {
  sums <- rbinom(n_sims, n, p)
  df <- data.frame(x = sums)
  
  # Normal approximation parameters
  mu <- n * p
  sigma <- sqrt(n * p * (1 - p))
  
  ggplot(df, aes(x = x)) +
    geom_histogram(aes(y = ..density..), binwidth = 1, fill = "seagreen", color = "white", alpha = 0.6) +
    stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), color = "red", size = 1) +
    labs(title = paste("Sum of", n, "Trials (p =", p, ")"),
         subtitle = "Green: Simulated Sums | Red: Normal Approximation",
         x = "Number of Successes", y = "Probability") +
    theme_light()
}

# Create three stages of the Binomial distribution
b1 <- binom_clt_plot(5)
b2 <- binom_clt_plot(20)
b3 <- binom_clt_plot(100)

# Display plots
grid.arrange(p_exp, b1, b2, b3, layout_matrix = rbind(c(1,1,1), c(2,3,4)))

