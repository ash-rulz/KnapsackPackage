#' Greedy Heuristic Approach For Knapsack Problem
#'
#' @param x 
#' @param W 
#'
#' @return
#' @export
#'
#' @examples
greedy_knapsack <- function(x, W){
  if(W <=0) stop("Invalid weight")
  
  init_mat <- NULL
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

RNGversion(min(as.character(getRversion()),"3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:1000000, size = n, replace = TRUE),
    v=runif(n = n, 0, 1000000)
  )
# x = matrix(data = c(2,3,5,7,1,4,1,10,5,15,7,6,18,3), nrow = 7, ncol = 2)
# colnames(x) <- c('w','v')
# W = 15
greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)

