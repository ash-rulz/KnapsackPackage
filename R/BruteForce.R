RNGversion(min(as.character(getRversion()),"3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
    )
str(knapsack_objects)

knapsack_dynamic <- function(x, W){
  df <- NULL
  
}
x = data.frame(w=c(2,3,4,5), v=c(1,2,5,6))
x
W <- 8
final_df <- data.frame(matrix(0, nrow = nrow(x)+1, ncol = W+1))
# first_row <- rep(0,W+1)
# final_df <- rbind(final_df, first_row)
final_df
for (i in 2:(nrow(x)+1)) {
  for (j in 2:(W+1)) {
    temp_var <- (j-x$w[i-1])
    if(temp_var<=0){
      final_df[i,j] <- final_df[i-1,j]
    }
    else{
      temp_val <- final_df[i-1,temp_var] + x$v[i-1]
      final_df[i,j] <- max(final_df[i-1,j], temp_val)
    }
  }
}
final_df
