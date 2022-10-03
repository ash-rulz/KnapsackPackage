#' Solving Knapsack Problem By Brute Force Method
#'
#' @param x 
#' @param W 
#'
#' @return
#' @import combinat
#' @import tidyr
#' @import dplyr
#' @export
#'
#' @examples
brute_force_knapsack <- function(x, W, paralellize = FALSE){
  # RNGversion(min(as.character(getRversion()),"3.5.3"))
  # set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
  # n <- 2000
  # knapsack_objects <-
  #   data.frame(
  #     w=sample(1:4000, size = n, replace = TRUE),
  #     v=runif(n = n, 0, 10000)
  #   )
  # x <- knapsack_objects[1:8,]
  # W <- 3500
  if (!is.data.frame(x)) {
    stop('Please input a dataframe for as input for x')
  }
  if (W <= 0) {
    stop('Please input a valid input')
  }
  if (paralellize == TRUE) {
    cores <- detectCores()
    cl1 <- makeCluster(cores, type = 'PSOCK')
    items_element <- data.frame(items = 1:nrow(x))
    w_sum_val <- parApply(cl = cl1, X = x, MARGIN = 2, FUN = function(x){
      colSums(combn(x,2))
    })
    # w_sum_val <- as.data.frame(w_sum_val)
    w_sum_val <- as.data.frame(w_sum_val)
    # w_sum_val
    colnames(w_sum_val) <- c("sum_wt_i", "sum_val_i")
    # w_sum_val
    stopCluster(cl1)
    
    cl2 <- makeCluster(cores, type = 'PSOCK')
    # clusterExport(cl1, varlist = 'items_element')
    item_comb <- parApply(cl = cl2, X = items_element, MARGIN = 2, FUN = function(y){
      as.data.frame(combn(items_element[,1],2))
    })
    # item_comb <- t(item_comb)
    item_comb <- t(as.data.frame.list(item_comb))
    item_comb <- as.data.frame(item_comb)
    colnames(item_comb) <- c('item1', 'item2')
    # item_comb
    stopCluster(cl2)
    
    w_sum_val <- cbind(w_sum_val, item_comb)
    row.names(w_sum_val) <- NULL
    # w_sum_val
    filter_wt <- filter(w_sum_val, sum_wt_i <= W)
    # filter_wt
    max_val_index <- which.max(filter_wt$sum_val_i)
    # max_val_index
    final_list  <- list(value=round(filter_wt$sum_val_i[max_val_index]),
                        elements = c(filter_wt$item1[max_val_index], filter_wt$item2[max_val_index])
    )
    final_list
    
  }else{
  item_df <- cbind(x, element_item = 1:nrow(x))
  item_df
  item_comb <- as.data.frame(combn(item_df[,3], 2))
  item_comb
  item_comb <- t(item_comb)
  item_comb <- as.data.frame(item_comb)
  item_comb
  class(item_comb)
  colnames(item_comb) <- c('item1', 'item2')
  item_comb
  item_comb_vec_df <- paste(item_comb$item1, item_comb$item2, sep = ',')
  item_comb_vec_df
  item_comb_vec_df <- as.data.frame(item_comb_vec_df)
  item_comb_vec_df
  # item_comb <- mutate(item_comb, item_comb_vec_df = paste(item1, item2))
  # item_comb[,3] <- as.numeric()
  # item_comb
  wt_comb <- as.data.frame(combn(x[,1], 2))
  wt_comb
  
  sum_wt <- colSums(wt_comb)
  sum_wt
  
  val_comb <- as.data.frame(combn(x[,2], 2))
  val_comb
  sum_val <- colSums(val_comb)
  sum_val
  
  library(dplyr)
  library(tidyverse)
  
  sum_wt_val_df <- data.frame()
  sum_wt_val_df <- rbind(sum_wt_val_df, sum_wt, sum_val)
  sum_wt_val_df <- t(sum_wt_val_df)
  sum_wt_val_df
  rownames(sum_wt_val_df) <- NULL
  sum_wt_val_df <- as.data.frame(sum_wt_val_df)
  colnames(sum_wt_val_df) <- c("sum_wt_i", "sum_val_i")
  sum_wt_val_df
  sum_wt_val_df <- cbind(sum_wt_val_df, item_comb)
  sum_wt_val_df
  class(sum_wt_val_df)
  
  filter_wt <- dplyr::filter(sum_wt_val_df, sum_wt_i <= W)
  filter_wt
  max_val_index <- which.max(filter_wt$sum_val_i)
  max_val_index
  final_list  <- list(value=round(filter_wt$sum_val_i[max_val_index]),
                      elements = c(filter_wt$item1[max_val_index], filter_wt$item2[max_val_index])
  )
  
  # final_list <- list(value = round(max(filter_wt$sum_val_i)), elements = filter_wt[filter_wt[['sum_val_i']]==max(filter_wt$sum_val_i), 'item_comb_vec_df'])
  
  final_list
  
  
  }
  return(final_list)
}
RNGversion(min(as.character(getRversion()),"3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500, paralellize = FALSE)
# 
# knapsack_objects
# 
# which.max(knapsack_objects$w)
# 
