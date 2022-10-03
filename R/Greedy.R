#' Greedy Heuristic Approach For Knapsack Problem
#'
#' @param x Data frame containing elements to be added to the knapsack
#' @param W Maximum weight of the knapsack
#'  
#'
#' @return A list containing two elements: value which contains the sum of the
#' elements and elements containing the set of elements accommodated in the 
#' knapsack
#' @export
#'
greedy_knapsack <- function(x, W){
  if(W <=0) stop("Invalid weight")
  
  init_mat <- NULL
  vByw <- NULL
  
  indx <- seq(from=1, to=nrow(x), by = 1)
  init_mat <- cbind(init_mat, indx)
  init_mat <- dplyr::bind_cols(init_mat,x)
  init_mat <- dplyr::mutate(init_mat, vByw = v/w)
  init_mat <- dplyr::arrange(init_mat, desc(vByw))
  w_resid <- W
  value <- 0L
  elements <- NULL
  for (i in 1:nrow(init_mat)) {
    if((w_resid-init_mat$w[i]) >= 0){
      value <- value + init_mat$v[i]
      elements <- c(elements, init_mat$indx[i])
    }
    else{
      break
    }
    w_resid <- w_resid - init_mat$w[i]
  }
  return(list(value = round(value), elements= elements))
}