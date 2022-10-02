library(parallel)
library(combinat)
library(dplyr)
cores <- detectCores()
cl1 <- makeCluster(cores, type = 'PSOCK')
RNGversion(min(as.character(getRversion()),"3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")

n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

x <- knapsack_objects[1:8,]
x
W <- 3500

# items_element = 1:nrow(x)
items_element <- data.frame(items = 1:nrow(x))
# class(items_element)
# item_element_comb <- data.frame()

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
clusterExport(cl2, varlist = 'items_element')
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

