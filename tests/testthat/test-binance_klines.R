library(binancer)

# Arguments 
from <- Sys.Date() - lubridate::days(365)
to <- Sys.Date()
interval <- "1d"

# ------------------------------------------  Test for api = "spot" ------------------------------------------
klines_api <- binance_klines(pair = "BTCUSDT", api = "spot", interval = interval, from = from, to = to)
test_that('Test 1: binance_klines(pair = "BTCUSDT", api = "spot", interval = interval, from = from, to = to)', {
  # Check if min(date) is equal to "from" date
  expect_equal(min(as.Date(klines_api$date)), from)
  # Check if max(date) is equal to "to" date
  expect_equal(max(as.Date(klines_api$date)), to)
  # Check "api" attribute
  expect_equal(attr(klines_api, "api"), "spot")
  # Check "interval" attribute
  expect_equal(attr(klines_api, "interval"), interval)
})


# ------------------------------------------  Test for api = "fapi" ------------------------------------------
klines_api <- binance_klines(pair = "BTCUSDT", api = "fapi", interval = interval, from = from, to = to)
test_that('Test 2: binance_klines(pair = "BTCUSDT", api = "fapi", interval = interval, from = from, to = to)', {
  # Check if min(date) is equal to "from" date
  expect_equal(min(as.Date(klines_api$date)), from)
  # Check if max(date) is equal to "to" date
  expect_equal(max(as.Date(klines_api$date)), to)
  # Check "api" attribute
  expect_equal(attr(klines_api, "api"), "fapi")
  # Check "interval" attribute
  expect_equal(attr(klines_api, "interval"), interval)
})


# ---------------------------------------------------------------  Test for api = "dapi" ---------------------------------------------------------------
klines_api <- binance_klines(pair = "BTCUSD_PERP", api = "dapi", interval = interval, uiKlines = FALSE, from = from, to = to)
test_that('Test 3: binance_klines(pair = "BTCUSD_PERP", api = "dapi", interval = interval, uiKlines = FALSE, from = from, to = to)', {
  # Check if min(date) is equal to "from" date
  expect_equal(min(as.Date(klines_api$date)), from)
  # Check if max(date) is equal to "to" date
  expect_equal(max(as.Date(klines_api$date)), to)
  # Check "api" attribute
  expect_equal(attr(klines_api, "api"), "dapi")
  # Check "interval" attribute
  expect_equal(attr(klines_api, "interval"), interval)
})

klines_api <- binance_klines(pair = "BTCUSD", api = "dapi", interval = interval, uiKlines = TRUE, contract_type = "perpetual", from = from, to = to)
test_that('Test 4: binance_klines(pair = "BTCUSD", api = "dapi", interval = interval, uiKlines = TRUE, contract_type = "perpetual", from = from, to = to)', {
  # Check if min(date) is equal to "from" date
  expect_equal(min(as.Date(klines_api$date)), from)
  # Check if max(date) is equal to "to" date
  expect_equal(max(as.Date(klines_api$date)), to)
  # Check "api" attribute
  expect_equal(attr(klines_api, "api"), "dapi")
  # Check "interval" attribute
  expect_equal(attr(klines_api, "interval"), interval)
})
