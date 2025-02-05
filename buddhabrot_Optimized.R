library(png)
library(parallel)
library(doParallel)
library(Rcpp)

# Single C++ code definition
escaped_cpp_code <- '
#include <Rcpp.h>
#include <complex>
#include <vector>

using namespace Rcpp;

// [[Rcpp::export]]
std::vector<std::complex<double>> escaped(std::complex<double> c, int max_iter) {
  std::complex<double> z(0.0, 0.0);
  std::vector<std::complex<double>> points;
  
  for (int i = 0; i < max_iter; ++i) {
    z = z * z + c;
    if (std::abs(z) > 2) return points;
    points.push_back(z);
  }
  return points;
}'

# Compile for main process
sourceCpp(code = escaped_cpp_code)

buddhabrot <- function(iterations = 1e6, size = 1000, max_iter = 1000) {
  points_matrix <- matrix(0, nrow = size, ncol = size)
  
  # Set up parallel backend
  num_cores <- detectCores() - 2
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  # Export required variables and compile on workers
  clusterExport(cl, c("escaped_cpp_code", "max_iter"), envir = environment())
  clusterEvalQ(cl, {
    library(Rcpp)
    sourceCpp(code = escaped_cpp_code)
  })
  
  start_time <- Sys.time()
  
  batch_size <- 1e5
  num_batches <- ceiling(iterations / batch_size)
  
  for (batch in 1:num_batches) {
    results <- foreach(i = 1:batch_size, .combine = 'c', .export = "max_iter") %dopar% {
      c_point <- complex(
        real = runif(1, -2, 2),
        imaginary = runif(1, -2, 2)
      )
      list(escaped(c_point, max_iter))
    }
    
    # Vectorized coordinate processing
    all_points <- unlist(results, recursive = FALSE)
    valid_points <- all_points[sapply(all_points, length) > 0]
    
    if (length(valid_points) > 0) {
      coords <- do.call(rbind, lapply(valid_points, function(points) {
        x <- floor((Re(points) + 2) * (size - 1) / 4) + 1
        y <- size - floor((Im(points) + 2) * (size - 1) / 4)
        cbind(y, x)
      }))
      
      valid <- coords[,1] >= 1 & coords[,1] <= size & 
        coords[,2] >= 1 & coords[,2] <= size
      points_matrix[coords[valid,]] <- points_matrix[coords[valid,]] + 1
    }
    
    gc()
  }
  
  # Proper cluster cleanup
  on.exit({
    stopCluster(cl)
    closeAllConnections()
  })
  
  # Normalization and output
  points_matrix <- log(points_matrix + 1)
  writePNG(points_matrix/max(points_matrix), "Buddhabrot_Optimized.png")
  
  cat("Elapsed time:", format(Sys.time() - start_time, digits = 3), "\n")
}

# Execute the function
buddhabrot(iterations = 1e6, size = 1500, max_iter = 1500)
