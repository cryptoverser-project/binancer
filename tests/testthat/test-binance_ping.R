library(binancer)


ping_api <- binance_ping(api = "spot")

test_that('Test 2: binance_ping(api = "spot")', {
  
  # Check Response
  expect_true(ping_api)

  # Check API attribute
  expect_equal(attr(ping_api, "api"), "spot")

})

ping_fapi <-  binance_ping(api = "fapi")

test_that('Test 2: binance_ping(api = "fapi")', {

  # Check Response
  expect_true(ping_fapi)

  # Check API attribute
  expect_equal(attr(ping_fapi, "api"), "fapi")

})

ping_dapi <- binance_ping(api = "dapi")

test_that('Test 2: binance_ping(api = "dapi")', {
  
  # Check Response
  expect_true(ping_dapi)

  # Check API attribute
  expect_equal(attr(ping_dapi, "api"), "dapi")

})

ping_eapi <- binance_ping(api = "eapi")

test_that('Test 2: binance_ping(api = "eapi")', {
  
  # Check Response
  expect_true(ping_eapi)

  # Check API attribute
  expect_equal(attr(ping_eapi, "api"), "eapi")

})
