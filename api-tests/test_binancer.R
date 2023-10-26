#library(binancer)


#query <- list(symbol = "BTCUSDT", limit = 1000)

#res <- binance_api(api = "spot", path = "depth", query = query)

#res$lastUpdateId

# Main Arguments 
pair <- "BTCUSDT"
interval <- "1d" 
from <-  as.POSIXct("2022-01-01 00:00:00")
to <- as.POSIXct("2022-01-10 00:00:00")

# Binance Klines 
source("R/binance_klines.R")
# SPOT
binance_spot_klines(pair = pair, interval = interval, from = from, to = to)
binance_spot_klines(pair = pair, interval = interval, from = from, to = to, uiKlines = TRUE)

# FAPI 
# Generic contracts 
binance_spot_klines(pair = pair, interval = interval, from = from, to = to, uiKlines = FALSE)
# Specify a contract 
binance_fapi_klines(pair = pair, interval = interval, from = from, to = to, uiKlines = TRUE, contract_type = "PERPETUAL")
binance_fapi_klines(pair = pair, interval = interval, from = from, to = to, uiKlines = TRUE, contract_type = "CURRENT_QUARTER")
binance_fapi_klines(pair = pair, interval = interval, from = from, to = to, uiKlines = TRUE, contract_type = "NEXT_QUARTER")

# DAPI 
# Generic contracts 
binance_dapi_klines(pair = "BTCUSD_PERP", interval = interval, from = from, to = to, uiKlines = FALSE)
binance_dapi_klines(pair = "ETHUSD_PERP", interval = interval, from = from, to = to, uiKlines = FALSE)
# Specify a contract 
binance_dapi_klines(pair = "BTCUSD", interval = interval, from = from, to = to, uiKlines = TRUE, contract_type = "PERPETUAL")
binance_dapi_klines(pair = "BTCUSD", interval = interval, from = from, to = to, uiKlines = TRUE, contract_type = "CURRENT_QUARTER")
binance_dapi_klines(pair = "BTCUSD", interval = interval, from = from, to = to, uiKlines = TRUE, contract_type = "NEXT_QUARTER")



