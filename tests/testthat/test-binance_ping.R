library(binancer)

# ------------  Test for api = "spot" ------------
ping_api <- binance_ping(api = "spot")
test_that('Test 1: binance_ping(api = "spot")', {
  # Check response
  expect_true(ping_api)
  # Check "api" attribute
  expect_equal(attr(ping_api, "api"), "spot")
})

# ------------  Test for api = "fapi" ------------
ping_api <-  binance_ping(api = "fapi")
test_that('Test 2: binance_ping(api = "fapi")', {
  # Check response
  expect_true(ping_api)
  # Check "api" attribute
  expect_equal(attr(ping_api, "api"), "fapi")
})

# ------------  Test for api = "dapi" ------------
ping_api <- binance_ping(api = "dapi")
test_that('Test 3: binance_ping(api = "dapi")', {
  # Check response
  expect_true(ping_api)
  # Check "api" attribute
  expect_equal(attr(ping_api, "api"), "dapi")
})

# ------------  Test for api = "eapi" ------------
ping_api <- binance_ping(api = "eapi")
test_that('Test 4: binance_ping(api = "eapi")', {
  # Check response
  expect_true(ping_api)
  # Check "api" attribute
  expect_equal(attr(ping_api, "api"), "eapi")
})
