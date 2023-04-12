# Load required library
library(tidyverse)

# Define the data
data <- tibble(
  Observation = 1:5,
  x1 = c(8, 9, 5, 4, 6),
  x2 = c(110, 115, 130, 124, 115)
)

# Define the initial centroids
centroid1 <- c(8, 110)
centroid2 <- c(9, 115)

# Function to calculate Euclidean distance
euclidean_distance <- function(x1, x2, centroid) {
  sqrt((x1 - centroid[1])^2 + (x2 - centroid[2])^2)
}

# Initialize previous clustering results to NULL
prev_cluster_results <- NULL

# Initialize iteration counter
iteration <- 1

# Iterate the clustering process
while (TRUE) {
  # Calculate distances to centroids
  data$dist_centroid1 <- with(data, euclidean_distance(x1, x2, centroid1))
  data$dist_centroid2 <- with(data, euclidean_distance(x1, x2, centroid2))
  
  # Print distance table
  cat("Iteration:", iteration, "\n")
  print(select(data, Observation, dist_centroid1, dist_centroid2))
  
  # Assign observations to clusters
  data$cluster <- ifelse(data$dist_centroid1 < data$dist_centroid2, 1, 2)
  
  # Check for convergence (no change in clustering)
  if (!is.null(prev_cluster_results) && identical(prev_cluster_results, data$cluster)) {
    break
  }
  
  # Update previous clustering results
  prev_cluster_results <- data$cluster
  
  # Calculate new centroids
  centroid1 <- colMeans(data[data$cluster == 1, c("x1", "x2")])
  centroid2 <- colMeans(data[data$cluster == 2, c("x1", "x2")])
  
  # Increment iteration counter
  iteration <- iteration + 1
}

# Print the final clustering results
cat("\nFinal clustering results:\n")
print(select(data, Observation, cluster))
