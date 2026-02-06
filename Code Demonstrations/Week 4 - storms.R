data(storms)

# Count unique storms per year
storms_per_year <- tapply(
  storms$name,
  storms$year,
  function(x) length(unique(x))
)

storms_per_year

lambda_hat <- mean(storms_per_year)
lambda_hat

# Histogram of observed counts
hist(
  storms_per_year,
  breaks = seq(min(storms_per_year) - 1,
               max(storms_per_year) + 1,
               by = 3),
  probability = TRUE,
  col = "lightgray",
  main = "Number of Atlantic Tropical Cyclones per Year 1975-2022",
  xlab = "Storm count",
  ylim = c(0, 0.12)
)

# Overlay Poisson pmf
k <- min(storms_per_year):max(storms_per_year)
points(k, dpois(k, lambda_hat), pch = 19)
lines(k, dpois(k, lambda_hat))

