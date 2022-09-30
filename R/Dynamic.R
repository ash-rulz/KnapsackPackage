RNGversion(min(as.character(getRversion()),"3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
    )
str(knapsack_objects)

seq_of_decision <- function(final_df, val){
  for (i in 1:nrow(final_df)) {
    if (val %in% final_df[i,]) {
      return(i)
      break
    }
  }
}

knapsack_dynamic <- function(x, W){
  #Creating empty data frame with the correction dimensions
  final_df <- data.frame(matrix(0, nrow = nrow(x)+1, ncol = W+1))
  
  #Calculating the values in the data-frame using the formula
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
  
  #Getting the correct indices to be returned using sequence of decisions
  temp_wt <- max(final_df[i,])
  final_index <- vector(mode = "integer", length = 0L)
  for (i in nrow(final_df):1) {
    if(temp_wt == 0) break
    temp_ind <- seq_of_decision(final_df, temp_wt)
    final_index <- c(final_index, temp_ind-1)
    temp_wt <- temp_wt - x$v[temp_ind-1]
  }
  
  #Formatting the final result
  final_index <- sort(final_index)
  final_result <- list(value = round(sum(x$v[as.vector(final_index)])),
                       elements = final_index)
  
}
lst <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
lst

v <- c(4,2)
sort(v)
########################################################################
x = data.frame(w=c(2,3,4,5), v=c(1,2,5,6))
x
W <- 8


final_df
#Creating empty data frame with the correction dimensions
final_df <- data.frame(matrix(0, nrow = nrow(x)+1, ncol = W+1))

#Calculating the values in the data-frame using the formula
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
temp_wt <- max(final_df[i,])
final_index <- NULL
for (i in nrow(final_df):1) {
  if(temp_wt == 0) break
  temp_ind <- seq_of_decision(final_df, temp_wt)
  final_index <<- c(final_index, temp_ind-1)
  temp_wt <- temp_wt - x$v[temp_ind-1]
}

final_result <- list(value = sum(x$v[as.vector(final_index)]),
                     elements = final_index)

final_result

seq_of_decision <- function(final_df, val){
  for (i in 1:nrow(final_df)) {
    if (val %in% final_df[i,]) {
      return(i)
      break
    }
  }
}
