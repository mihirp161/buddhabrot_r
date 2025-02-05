library(png)
library(parallel)
library(doParallel)
library(foreach)

escaped <- function(c, max_iter = 1000) {
  z <- complex(real = 0, imaginary = 0)
  points <- vector("list", max_iter)
  
  for (i in 1:max_iter) {
    z <- z^2 + c
    if (Mod(z) > 2) {
      return(NULL)
    }
    points[[i]] <- z
  }
  
  unlist(points)
}

buddhabrot <- function(iterations = 1e6, size = 1000, max_iter = 1000) {
  points_matrix <- matrix(0, nrow = size, ncol = size)
  
  # Set up parallel backend
  num_cores <- detectCores() - 1  # Reserve one core for system responsiveness
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  start_time <- Sys.time()
  
  # Use foreach for parallel processing
  results <- foreach(i = 1:iterations, .combine = 'c', .packages = c('png'), .export = c('escaped')) %dopar% {
    real <- runif(1, -2, 2)
    imag <- runif(1, -2, 2)
    c_point <- complex(real = real, imaginary = imag)
    
    result_points <- escaped(c_point, max_iter)
    if (is.null(result_points)) return(NULL)
    
    list(result_points)
  }
  
  stopCluster(cl)  # Properly stop the cluster
  
  # Process the results
  for (res in results) {
    if (is.null(res)) next
    points <- res[[1]]
    # Convert complex numbers to coordinates
    x_coords <- floor(size * (Re(points) + 2) / 4) + 1
    y_coords <- floor(size * (Im(points) + 2) / 4) + 1
    
    # Filter valid coordinates
    valid_indices <- which(x_coords > 0 & x_coords <= size & y_coords > 0 & y_coords <= size)
    x_valid <- x_coords[valid_indices]
    y_valid <- y_coords[valid_indices]
    
    # Increment the counts
    index <- cbind(x_valid, y_valid)
    points_matrix[index] <- points_matrix[index] + 1
  }
  
  # Normalize the matrix and adjust brightness and contrast
  points_matrix[points_matrix > 0] <- log(points_matrix[points_matrix > 0] + 1)
  max_val <- max(points_matrix)
  
  # Create a black and white image
  img <- array(0, dim = c(size, size))
  for (i in 1:size) {
    for (j in 1:size) {
      val <- points_matrix[i, j] / max_val
      img[i, j] <- ifelse(val > 0, val, 0)
    }
  }
  
  # Save the image
  writePNG(img, "Buddhabrot_BlackAndWhite_simple.png")
  cat("Image saved as Buddhabrot_BlackAndWhite_simple.png\n")
  
  elapsed <- difftime(Sys.time(), start_time, units = "secs")
  cat(sprintf("Total Elapsed time: %.2f seconds\n", elapsed))
}

# Run the function
buddhabrot()
