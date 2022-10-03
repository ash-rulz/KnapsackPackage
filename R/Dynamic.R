#' Helper Method To Get The Correct Indexes
#'
#' @param final_mat Final matrix of values
#' @param val Final value
#'
#' @return A list containing two elements: value which contains the sum of the
#' elements and elements containing the set of elements accommodated in the 
#' knapsack
#' @export
#'
#' @examples
seq_of_decision <- function(final_mat, val){
  for (i in 1:nrow(final_mat)) {
    if (val %in% final_mat[i,]) {
      return(i)
      break
    }
  }
}

#' Solving Knapsack Problem By Dynamic Method
#'
#' @param x Data frame containing elements to be added to the knapsack
#' @param W Maximum weight of the knapsack
#'
#'
#' @return
#' @export
#'
#' @examples
knapsack_dynamic <- function(x, W){
  #Creating empty data frame with the correction dimensions
  final_mat <- matrix(0, nrow = nrow(x)+1, ncol = W+1)
  
  #Calculating the values in the data-frame using the formula
  for (i in 2:(nrow(x)+1)) {
    for (j in 2:(W+1)) {
      temp_var <- (j-x$w[i-1])
      if(temp_var<=0){
        final_mat[i,j] <- final_mat[i-1,j]
      }
      else{
        temp_val <- final_mat[i-1,temp_var] + x$v[i-1]
        final_mat[i,j] <- max(final_mat[i-1,j], temp_val)
      }
    }
  }
  
  #Getting the correct indices to be returned using sequence of decisions
  temp_wt <- max(final_mat[i,])
  final_index <- vector(mode = "integer", length = 0L)
  for (i in nrow(final_mat):1) {
    if(temp_wt == 0) break
    temp_ind <- seq_of_decision(final_mat, temp_wt)
    final_index <- c(final_index, temp_ind-1)
    temp_wt <- temp_wt - x$v[temp_ind-1]
  }
  
  #Formatting the final result
  final_index <- sort(final_index)
  final_result <- list(value = round(sum(x$v[as.vector(final_index)])),
                       elements = final_index)
  
}
RNGversion(min(as.character(getRversion()),"3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )
system.time(lst <- knapsack_dynamic(x = knapsack_objects[1:8,],
                                    W = 3500))
# It takes around 6 seconds