
weight_ip <- function(object){
  
  if(missing(object)){
    
    df_weight <- dplyr::bind_rows(binance_env[['weight_ip']])
    
    return(df_weight)
  }
  
  object_api <- attr(object, "api")
  
  new_weight <- binance_env[['weight_ip']][[object_api]] + attr(object, "ip_weight")
  
  binance_env[['weight_ip']][[object_api]] <- new_weight
  
  binance_env[['weight_ip']][['total']] <- sum(unlist(binance_env[['weight_ip']]))
  
}




