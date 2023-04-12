

##    1)
##    
##                       Centroid1        Centroid2
##    
##               1           0              5.10
##    
##               2           5.10           0   
##    
##               3          20.2           15.5 
##    
##               4          14.6           10.3 
##    
##               5           5.39           3  
##    
##    
##    
##    
##    Cluster 1: Observation 1
##    
##    Cluster 2: Observations 2,3,4,5
##    
##    
##    
##    2)
##    
##                       Centroid1        Centroid2
##    
##               1           0             11.2 
##    
##               2           5.10           6.71
##    
##               3          20.2            9.06
##    
##               4          14.6            3.61
##    
##               5           5.39           6 
##    
##    
##    
##    
##    Cluster 1: Observation 1,2,5
##    
##    Cluster 2: Observations 3,4
##    
##    
##    3)
##    
##    
##                        Centroid1        Centroid2
##               1           3.35          17.4 
##    
##               2           2.13          12.8 
##    
##               3          16.9            3.04
##    
##               4          11.3            3.04
##    
##               5           2.36          12.1 
##    
##    
##    
##    
##    Cluster 1: Observation 1,2,5
##    
##    Cluster 2: Observations 3,4


library(tidyverse)

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

prev_cluster_results <- NULL
iteration <- 1
while (TRUE) {
  data$dist_centroid1 <- with(data, euclidean_distance(x1, x2, centroid1))
  data$dist_centroid2 <- with(data, euclidean_distance(x1, x2, centroid2))
  cat("Iteration:", iteration, "\n")
  print(select(data, Observation, dist_centroid1, dist_centroid2))
  data$cluster <- ifelse(data$dist_centroid1 < data$dist_centroid2, 1, 2)
  # Check for convergence (no change in clustering)
  if (!is.null(prev_cluster_results) && identical(prev_cluster_results, data$cluster)) {
    break
  }
  prev_cluster_results <- data$cluster
  centroid1 <- colMeans(data[data$cluster == 1, c("x1", "x2")])
  centroid2 <- colMeans(data[data$cluster == 2, c("x1", "x2")])
  iteration <- iteration + 1
}
cat("\nFinal clustering results:\n")
print(select(data, Observation, cluster))
