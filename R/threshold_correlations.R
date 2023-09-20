# Load necessary libraries
library(grid)
library(grDevices)
#'
#' #' @importFrom stats cor
#' @importFrom graphics image axis par
#' @importFrom grDevices colorRampPalette

#' Threshold-based correlation calculation
#'
#' @param x A numeric vector.
#' @param y A numeric vector; must have the same length as `x`.
#' @param type Character string; type of correlation table to calculate.
#'   Can be one of "HH", "LL", "HL", "LH", or "all".
#' @param support Numeric; minimum percentage of observations needed to report a correlation value.
#' @param grid_size Numeric; the size of the grid for the correlation table.
#' @param threshold_vectors List; optional custom vectors of thresholds for x and y.
#' 
#' @return A list of class "tcorr" containing the correlation tables and threshold vectors.
#' 
#' @seealso \code{\link{tables}}, \code{\link{thresholds}}, \code{\link{statistics}}, \code{\link{display}}
#' 
#' @examples
#' \dontrun{
#' result <- tcorr(runif(100), runif(100))
#' }
#' @export
tcorr <- function(x, y, type = c("all", "HH", "LL", "HL", "LH"), support = 0.1, grid_size = 12, threshold_vectors = NULL) {
  
  # Check that vectors have the same length
  if (length(x) != length(y)) {
    stop("Vectors x and y must have the same length.")
  }
  
  # Create grid if threshold_vectors are not provided
  if (is.null(threshold_vectors)) {
    threshold_vectors <- list(x = seq(min(x), max(x), length.out = grid_size),
                              y = seq(min(y), max(y), length.out = grid_size))
  }
  
  # Initialize result list
  result <- list()
  
  # Calculate the minimum number of observations based on support
  min_obs <- ceiling(length(x) * support)
  
  # Function to calculate correlation for a specific type
  calc_corr <- function(type, x, y, thresh_x, thresh_y) {
    subset_x <- NULL
    subset_y <- NULL
    
    if (type == "HH") {
      valid_indices <- which(x >= thresh_x & y >= thresh_y)
    } else if (type == "LL") {
      valid_indices <- which(x <= thresh_x & y <= thresh_y)
    } else if (type == "HL") {
      valid_indices <- which(x >= thresh_x & y <= thresh_y)
    } else if (type == "LH") {
      valid_indices <- which(x <= thresh_x & y >= thresh_y)
    }
    
    subset_x <- x[valid_indices]
    subset_y <- y[valid_indices]
    
    # Check for minimum support
    if (length(subset_x) < min_obs || length(subset_y) < min_obs) {
      return(NA)
    }
    
    return(cor(subset_x, subset_y, use = "complete.obs"))
  }
  
  # Loop through the types and calculate correlations accordingly
  types_to_calculate <- if(type == "all") c("HH", "LL", "HL", "LH") else type
  for(curr_type in types_to_calculate) {
    result[[curr_type]] <- calc_corr(curr_type, x, y, threshold_vectors$x, threshold_vectors$y)
  }
  
  # Additional components
  result$thresholds <- threshold_vectors
  
  class(result) <- "tcorr"
  
  return(result)
}

#' Retrieve specified correlation table
#'
#' @param obj A "tcorr" object.
#' @param type Character string; type of the correlation table to retrieve.
#'
#' @return The correlation table matrix.
#' 
#' @seealso \code{\link{tcorr}}, \code{\link{thresholds}}, \code{\link{statistics}}, \code{\link{display}}
#' 
#' @examples
#' \dontrun{
#' tables(result, "HH")
#' }
#' @export
tables <- function(obj, type) {
  return(obj[[type]])
}

#' Retrieve threshold vectors
#'
#' @param obj A "tcorr" object.
#' 
#' @return List; the threshold vectors used in `obj`.
#' 
#' @seealso \code{\link{tcorr}}, \code{\link{tables}}, \code{\link{statistics}}, \code{\link{display}}
#' 
#' @examples
#' \dontrun{
#' thresholds(result)
#' }
#' @export
thresholds <- function(obj) {
  return(obj$thresholds)
}

#' Compute summary statistics
#'
#' @param obj A "tcorr" object.
#' @param type Character string; type of the correlation table for which to compute statistics.
#' 
#' @return List; summary statistics for the specified correlation table, including minimum, maximum, and average correlation values.
#' 
#' @seealso \code{\link{tcorr}}, \code{\link{tables}}, \code{\link{thresholds}}, \code{\link{display}}
#' 
#' @examples
#' \dontrun{
#' statistics(result, "HH")
#' }
#' @export
statistics <- function(obj, type) {
  tbl <- obj[[type]]
  return(list(min = min(tbl, na.rm = TRUE), max = max(tbl, na.rm = TRUE), avg = mean(tbl, na.rm = TRUE)))
}


#' Retrieve threshold index associations
#'
#' @param obj A "tcorr" object.
#' @param type Character string; type of the correlation table.
#' 
#' @return Summary of which row/column corresponds to which threshold.
#' 
#' @seealso \code{\link{tcorr}}, \code{\link{tables}}, \code{\link{thresholds}}, \code{\link{statistics}}, \code{\link{display}}
#' 
#' @examples
#' \dontrun{
#' thrh_index_assoc(result, "HH")
#' }
#' @export
thrh_index_assoc <- function(obj, type) {
  # ...
}

#' Display specified correlation table
#'
#' @param obj A "tcorr" object.
#' @param type Character string; type of the correlation table to display. Explained in detail in \code{\link{tcorr}}.
#' 
#' @return None; prints the correlation table.
#' 
#' @seealso \code{\link{tcorr}}, \code{\link{tables}}, \code{\link{thresholds}}, \code{\link{statistics}}
#' 
#' @examples
#' \dontrun{
#' display(result, "HH")
#' }
#' @export
display <- function(obj, type) {
  if (!is(obj, "tcorr")) {
    stop("The object is not of class 'tcorr'")
  }
  
  mat <- obj[[type]]
  
  if (is.null(mat)) {
    cat("No table available for the specified type:", type)
    return(NULL)
  }
  
  # Define continuous colors from blue to red for values in [-1, 1]
  cols <- c("white", colorRampPalette(c("blue", "white", "red"))(256))  # Added white for NA
  
  # Create a break vector that includes NA
  breaks <- c(-2, seq(-1, 1, length.out = 256))
  
  # Replace NA with -2 to make it white
  mat[is.na(mat)] <- -2
  
  # Main plot
  par(mar = c(5, 5, 4, 8))
  image(
    z = mat, 
    col = cols, 
    breaks = breaks, 
    xaxt = 'n', 
    yaxt = 'n',
    xlab = "Thresholds for x",
    ylab = "Thresholds for y"
  )
  
  title(main = paste("Threshold Correlation Table (Type:", type, ")"))
  
  axis(1, at = seq(0, 1, length.out = length(obj$thresholds$x)), labels = obj$thresholds$x)
  axis(2, at = seq(0, 1, length.out = length(obj$thresholds$y)), labels = obj$thresholds$y)
  
  # Add text to cells
  n_row <- nrow(mat)
  n_col <- ncol(mat)
  for(i in 1:n_row) {
    for(j in 1:n_col) {
      cell_val <- mat[i, j]
      if (cell_val != -2) {  # Skip NAs (which are coded as -2)
        text((i - 1) / (n_row - 1), (j - 1) / (n_col - 1), sprintf("%.2f", cell_val), cex = 0.8)
      }
    }
  }
  
  # Add color bar
  par(mar = c(5, 0, 4, 1))
  image(
    z = matrix(seq(-1, 1, length.out = 256), ncol = 1), 
    col = cols[-1],  # Exclude the color for NA
    breaks = seq(-1, 1, length.out = 257),
    xaxt = 'n',
    yaxt = 'n'
  )
  
  axis(4, at = seq(0, 1, by = 0.2), labels = seq(-1, 1, by = 0.2))
}

