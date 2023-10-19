
add_moving_average <- function(data, cols = "close", ma.type = c("sma", "ema"), n = 12){
  
  ma.type <- match.arg(ma.type, choices = c("sma", "ema"))
  
  for( i in 1:length(cols)){
    for(j in 1:length(n)){
      
      old_col_name = cols[i]
      new_col_name <- paste0(old_col_name,"_", tolower(ma.type), "_", n[j])
      data[[new_col_name]] <- NA_integer_
  
      if(ma.type == "sma"){
        data[[new_col_name]] <- TTR::SMA(data[[old_col_name]], n = n[j]) 
      } else if(ma.type == "ema"){
        data[[new_col_name]] <- TTR::EMA(data[[old_col_name]], n = n[j]) 
      }
    }
  }
  return(data)
}


cross_over <- function(x, y){
  
  if(length(x) != length(y)){
    warning("x and y have not same length!")
    return(NULL)
  }

  x_lag_1 <- stats::lag(x, 1)  
  y_lag_1 <- stats::lag(y, 1)  
  
  condition <- x_lag_1 < y_lag_1 & x > y 
  condition[1] <- FALSE 
  
  return(condition)
}

cross_under <- function(x, y){
  
  if(length(x) != length(y)){
    warning("x and y have not same length!")
    return(NULL)
  }
  
  x_lag_1 <- stats::lag(x, 1)  
  y_lag_1 <- stats::lag(y, 1)  
  
  condition <- x_lag_1 > y_lag_1 & x < y
  condition[1] <- FALSE 
  
  return(condition)
}
