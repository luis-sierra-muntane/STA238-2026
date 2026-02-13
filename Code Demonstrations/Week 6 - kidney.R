kidney <- read.table("kidney.txt", header = TRUE)

# Calculate the Pearson correlation coefficient
rho <- cor(kidney$age, kidney$tot)

# Create the scatterplot
plot(kidney$age, kidney$tot, 
     pch = 20, 
     col = "steelblue",
     xlab = "Age", 
     ylab = "Kidney Function (tot)",
     main = "Kidney Function vs. Age")

# Fit the linear model (Line of Best Fit)
fit <- lm(tot ~ age, data = kidney)

# Add the regression line to the plot
abline(fit, col = "red", lwd = 2)

B <- 1000
set.seed(123) # For reproducibility

boot_corrs <- replicate(B, {
  # Resample the row indices with replacement
  resample_indices <- sample(1:nrow(kidney), replace = TRUE)
  sample_data <- kidney[resample_indices, ]
  
  # Calculate correlation for this bootstrap sample
  cor(sample_data$age, sample_data$tot)
})

# Calculate the Standard Error (Standard Deviation of the bootstrap distribution)
se_boot <- sd(boot_corrs)

# 5. Add results to the plot
text_label <- paste("r =", round(rho, 3), 
                    "\nBootstrap SE =", round(se_boot, 3))
text(x = 70, y = 4, labels = text_label, cex = 1, font = 2)

# Print summary to console
cat("Observed Correlation:", round(rho, 4), "\n")
cat("Bootstrap SE (B=2000):", round(se_boot, 4), "\n")

