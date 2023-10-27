summary.depth <- function(data, depth = 0.02) {
  
  output <- data[1,1:3]
  # ASK (sellers)
  ask_data <- dplyr::filter(data, side == "ASK") 
  ask_data <- dplyr::arrange(ask_data, price)
  # BID (buyers)
  bid_data <- dplyr::filter(data, side == "BID")
  bid_data <- dplyr::arrange(bid_data, dplyr::desc(price))
  
  # Best BID-ASK 
  ask <- ask_data[1,]
  bid <- bid_data[1,]
  output$p_ask <- ask$price
  output$q_ask <- ask$quantity
  output$p_bid <- bid$price
  output$q_bid <- bid$quantity
  # Microprice, imbalance and spread
  output <- dplyr::mutate(output, 
                          p_mp = (p_ask*q_ask + p_bid*q_bid)/(q_ask + q_bid), 
                          i_bid = (q_bid - q_ask)/(q_ask + q_bid),
                          s = p_ask - p_bid)
  
  # Depth data 
  ask_depth <- dplyr::filter(ask_data, price <= output$p_ask*(1+depth)) # +depth%
  bid_depth <- dplyr::filter(bid_data, price <= output$p_bid*(1-depth)) # -depth%
  output$ask_d <- sum(ask_depth$price*ask_depth$quantity)
  output$bid_d <- sum(bid_depth$price*bid_depth$quantity)
  output$i_bid_depth <- (output$bid_d - output$ask_d)/(output$bid_d + output$ask_d)

  return(output)
}

