library(png)
library(parallel)
library(doParallel)
library(Rcpp)

# Define the escaped function using Rcpp
Rcpp::cppFunction('
std::vector<std::complex<double>> escaped(std::complex<double> c, int max_iter = 1000) {
  std::complex<double> z(0, 0);
  std::vector<std::complex<double>> points;
  
  for (int i = 0; i < max_iter; ++i) {
    z = z * z + c;
    if (std::abs(z) > 2) {
      return points;
    }
    points.push_back(z);
  }
  return points;
}')

# Define the buddhabrot function
buddhabrot <- function(iterations = 1e6, size = 1000, max_iter = 1000) {
  points_matrix <- matrix(0, nrow = size, ncol = size)
  
  # Set up parallel backend
  num_cores <- detectCores() - 2  # Use fewer cores
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  # Compile the escaped function on ALL workers
  clusterEvalQ(cl, {
    library(Rcpp)
    Rcpp::cppFunction('
      std::vector<std::complex<double>> escaped(std::complex<double> c, int max_iter = 1000) {
        std::complex<double> z(0, 0);
        std::vector<std::complex<double>> points;
        for (int i = 0; i < max_iter; ++i) {
          z = z * z + c;
          if (std::abs(z) > 2) return points;
          points.push_back(z);
        }
        return points;
      }')
  })
  
  start_time <- Sys.time()
  
  # Parallel loop with smaller batches
  batch_size <- 1e5
  num_batches <- ceiling(iterations / batch_size)
  
  for (batch in 1:num_batches) {
    results <- foreach(i = 1:batch_size, .packages = c('Rcpp'), .export = NULL) %dopar% {
      real <- runif(1, -2, 2)
      imag <- runif(1, -2, 2)
      c_point <- complex(real = real, imaginary = imag)
      
      result_points <- escaped(c_point, max_iter)
      if (length(result_points) == 0) return(NULL)
      
      list(result_points)
    }
    
    # Process results
    for (res in results) {
      if (is.null(res)) next
      points <- res[[1]]
      
      x_coords <- floor((Re(points) + 2) * (size - 1) / 4) + 1
      y_coords_unflipped <- floor((Im(points) + 2) * (size - 1) / 4) + 1
      y_coords <- size + 1 - y_coords_unflipped  # Flip y-axis
      
      valid_indices <- which(x_coords >= 1 & x_coords <= size & y_coords >= 1 & y_coords <= size)
      x_valid <- x_coords[valid_indices]
      y_valid <- y_coords[valid_indices]
      
      points_matrix[cbind(y_valid, x_valid)] <- points_matrix[cbind(y_valid, x_valid)] + 1
    }
    
    gc()  # Explicit garbage collection
  }
  
  stopCluster(cl)  # Clean up connections
  
  # Normalize and save image
  points_matrix[points_matrix > 0] <- log(points_matrix[points_matrix > 0] + 1)
  img <- points_matrix / max(points_matrix)
  writePNG(img, "Buddhabrot_BlackAndWhite_rcpp.png")
  
  elapsed <- difftime(Sys.time(), start_time, units = "secs")
  cat(sprintf("Elapsed time: %.2f seconds\n", elapsed))
}

# Run the function with adjusted parameters
buddhabrot(iterations = 1e7, size = 1000, max_iter = 1500)# Start with smaller parameters
