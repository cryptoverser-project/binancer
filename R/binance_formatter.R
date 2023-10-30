binance_formatter <- function(data){
  
  if (purrr::is_empty(data)){
    return(dplyr::tibble())
  }
  
  # POSIXct columns 
  date_columns <- list(
    date = "date",
    openTime = "date",
    time = "date",
    timestamp = "date", 
    updateTime = "date",
    date_close = "date_close",
    closeTime = "date_close",
    expiryDate = "expiry_date"
  )
  # Numeric columns
  numeric_columns <- list(
    id = "trade_id",
    agg_id = "agg_id",
    firstId = "first_id",
    firstTradeId = "first_id",
    lastId = "last_id",
    last_update_id = "last_update_id",
    first_update_id = "first_update_id",
    orderId = "order_id",
    orderListId = "order_list_id",
    price = "price",
    askPrice = "ask",
    bidPrice = "bid",
    ask_quantity = "askQty",
    bid_quantity = "bidQty",
    quantity = "quantity",
    qty = "quantity",
    open = "open",
    openPrice = "open",
    high = "high",
    highPrice = "high",
    low = "low",
    lowPrice = "low",
    close = "close",
    lastPrice = "last",
    prevClosePrice = "last_close",
    weightedAvgPrice = "weighted_price",
    volume = "volume",
    volume_quote = "volume_quote",
    quoteVolume = "volume_quote",
    trades = "trades",
    tradeCount = "trades",
    count = "trades",
    takerVolume = "taker_volume",
    takerAmount = "taker_amount", 
    amount = "amount",
    taker_buy = "taker_buy",
    taker_buy_quote = "taker_buy_quote",
    strikePrice = "strike",
    exercisePrice = "exercise_price",
    openInterest = "open_interest",
    sumOpenInterest = "open_interest",
    sumOpenInterestUsd = "open_interest_usd",
    sumOpenInterestValue = "open_interest_usd",
    uid = "user_id",
    maker = "maker", 
    taker = "taker",
    buyer = "buyer",
    seller = "seller",
    free = "free",
    locked = "locked",
    makerCommission = "maker_commission",
    takerCommission = "taker_commission",
    buyerCommission = "buyer_commission",
    sellerCommission = "seller_commission",
    commission = "commission",
    buySellRatio = "buy_sell_ratio",
    sellVol = "sell_vol",
    buyVol = "buy_vol",
    longAccount = "long_account",
    longPosition = "long_position",
    shortAccount = "short_account",
    shortPosition = "short_position",
    longShortRatio = "long_short_account",
    takerSellVol = "taker_sell_volume",
    takerSellVolValue = "taker_sell_vol_value",
    takerBuyVol = "taker_buy_vol",
    takerBuyVolValue = "taker_buy_vol_value"
    )
  
  # Character columns
  character_columns <- list(
    pair = "pair",
    symbol = "pair",
    asset = "symbol",
    indicator = "indicator",
    status = "status", 
    market = "market",
    side = "side",
    accountType = "account_type",
    permissions = "permissions",
    isBuyerMaker = "side",
    isBuyer = "side",
    isMaker = "is_maker",
    canTrade = "can_trade",
    canWithdraw = "can_withdraw",
    canDeposit = "can_deposit",
    brokered = "brokered",
    requireSelfTradePrevention = "require_self_trade_prevention",
    preventSor = "prevent_sor",
    commissionAsset = "commission_asset"
  )

  # Excluded columns
  exclude_columns <- list(
    ignore = "ignore",
    isBestMatch = "isBestMatch",
    contractType = "contractType"
  )
 
  # Create a list with all columns 
  col_names <- append(date_columns, character_columns)
  col_names <- append(col_names, numeric_columns)
  col_names <- append(col_names, exclude_columns)
  # Search for new names 
  new_col_names <- col_names[names(col_names) %in% colnames(data)]
  # Reorder the columns reflecting old names orders  
  data <- data[, names(new_col_names)]
  # Extract new columns names 
  new_col_names <- unlist(new_col_names)
  
  # Convert numeric columns 
  for (v in names(numeric_columns)) {
    column <- data[[v]]
    if (!is.null(column)){
      data[[v]] <- as.numeric(data[[v]])
    }
  }
  # Convert POSIXct columns 
  for (v in names(date_columns)) {
    column <- data[[v]]
    if (!is.null(column)){
      data[[v]] <- as.POSIXct(as.numeric(data[[v]])/1000,  origin = "1970-01-01")
    }
  }
  # Assign new names 
  if (!purrr::is_empty(new_col_names)){
    colnames(data) <- new_col_names
  }
  # Remove excluded columns  
  idx_exclude_columns <- !(colnames(data) %in% names(exclude_columns))
  data <- data[,idx_exclude_columns]
  return(data)
}

