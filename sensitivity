## Examine the sensitivity of different features on the outcome ##
## If the feature is continuous, I add one standard deviation to each observation (for the feature) and compute the average percentage change in the outcome ##
## If the feature is binary, I replace 0 with 1 and 1 with 0, and compute the average percentage change in the outcome ##
get_sentitivity <- function(data, columns, model){
  sentitivity <- data_frame(columns)
  sentitivity$perc_change <- 0
  
  mean_y <- mean(model$fitted.values)
  X = data %>% dplyr::select(columns)
  X = na.omit(X)
  
  for (i in 1:length(columns)){
    X_new <- X
    if(length(unique(X_new[,columns[i]])) <= 2){
      X_new[,columns[i]] <- ifelse(X_new[,columns[i]] == 1, 0, 1)
      predict_new <- predict(model, newdata = X_new, se.fit=TRUE, type='response')
      sentitivity$perc_change[i] <- (mean(predict_new$fit) - mean_y) / mean_y
    } else {
      X_new[,columns[i]] <- X_new[,columns[i]] + sd(X_new[,columns[i]])
      predict_new <- predict(model, newdata = X_new, se.fit=TRUE, type='response')
      sentitivity$perc_change[i] <- (mean(predict_new$fit) - mean_y) / mean_y
    }
  }
  return(sentitivity)
}
