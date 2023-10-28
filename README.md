# Wrapper for Binance REST API in R

The "binancer" package is part of a collection of development tools designed for retrieving cryptocurrency data. It enables you to access various data related to cryptocurrency pairs through the [Binance API](https://binance-docs.github.io/apidocs). 
More details and examples documenting the functions can be found at [cryptoverser.org](https://cryptoverser.org/docs).


# Installation 

```{r Installation}
# lightweight
remotes::install_github("cryptoverser-project/binancer")
# or
devtools::install_github("cryptoverser-project/binancer")

library(binancer)
```

## OHLCV Historical Data

Klines data contains high, low, open, close and volumes data. The binance api allows to retrieve this kind of data for many time frames ("1s", "1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "6h", "8h", "12h", "1d", "3d", "1w", and "1M"). For example: 

```{r}
binance_klines(api = "spot", pair = "BTCUSDT", interval = "1h")

# A tibble: 24 × 13
   date                date_close          market pair      open   high    low  close volume volume_quote trades taker_buy taker_buy_quote
   <dttm>              <dttm>              <chr>  <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl>        <dbl>  <dbl>     <dbl>           <dbl>
 1 2023-10-19 22:00:00 2023-10-19 22:59:59 spot   BTCUSDT 28751. 28882. 28685. 28715.  1737.    49976234.  59830      837.       24072439.
 2 2023-10-19 23:00:00 2023-10-19 23:59:59 spot   BTCUSDT 28715. 28740. 28604. 28604.  1119.    32056663.  42623      432.       12368607.
 3 2023-10-20 00:00:00 2023-10-20 00:59:59 spot   BTCUSDT 28604. 28705. 28599. 28650.   768.    22004203.  28479      388.       11125072.
 4 2023-10-20 01:00:00 2023-10-20 01:59:59 spot   BTCUSDT 28650. 28718. 28630. 28714.   613.    17582981.  27099      338.        9705128.
 5 2023-10-20 02:00:00 2023-10-20 02:59:59 spot   BTCUSDT 28714. 28772. 28636. 28640    886.    25431529.  41341      477.       13692333.
 6 2023-10-20 03:00:00 2023-10-20 03:59:59 spot   BTCUSDT 28640. 28660. 28578. 28660.   641.    18354126.  34806      305.        8715906.
 7 2023-10-20 04:00:00 2023-10-20 04:59:59 spot   BTCUSDT 28659. 29123  28641. 28980   3294.    95260547. 111096     1912.       55290507.
 8 2023-10-20 05:00:00 2023-10-20 05:59:59 spot   BTCUSDT 28980  29400  28920  29235.  4343.   126753071. 150111     2394.       69872969.
 9 2023-10-20 06:00:00 2023-10-20 06:59:59 spot   BTCUSDT 29235. 29350  29175. 29319.  2399.    70198015.  87783     1263.       36968173.
10 2023-10-20 07:00:00 2023-10-20 07:59:59 spot   BTCUSDT 29319. 29346. 29111. 29222.  3774.   110132371.  86502     1298.       37904423.
# 14 more rows
```

```{r}
binancer::binance_klines(api = "spot", pair = "BTCUSDT", from = "2019-01-05", to = "2023-07-19", interval = "1d")

# A tibble: 1,657 × 13
   date                date_close          market pair     open  high   low close volume volume_quote trades taker_buy taker_buy_quote
   <dttm>              <dttm>              <chr>  <chr>   <dbl> <dbl> <dbl> <dbl>  <dbl>        <dbl>  <dbl>     <dbl>           <dbl>
 1 2019-01-05 01:00:00 2019-01-06 00:59:59 spot   BTCUSDT 3790. 3841. 3751  3771. 30491.   115893501. 203673    14909.       56667455.
 2 2019-01-06 01:00:00 2019-01-07 00:59:59 spot   BTCUSDT 3771. 4028. 3740  3988. 36554.   142198812. 240496    19773.       76903706.
 3 2019-01-07 01:00:00 2019-01-08 00:59:59 spot   BTCUSDT 3988. 4018. 3922. 3975. 31870.   126830380. 221219    16404.       65287612.
 4 2019-01-08 01:00:00 2019-01-09 00:59:59 spot   BTCUSDT 3977. 4070. 3903  3955. 38901.   154778847. 242898    20048.       79781105.
 5 2019-01-09 01:00:00 2019-01-10 00:59:59 spot   BTCUSDT 3955. 4007. 3930. 3967. 28989.   115218950. 209240    14984.       59559011.
 6 2019-01-10 01:00:00 2019-01-11 00:59:59 spot   BTCUSDT 3966. 3996. 3540  3586. 59402.   221789348. 374760    28288.      105616252.
 7 2019-01-11 01:00:00 2019-01-12 00:59:59 spot   BTCUSDT 3586. 3658  3465  3601. 38339.   137560744. 264996    19907.       71430016.
 8 2019-01-12 01:00:00 2019-01-13 00:59:59 spot   BTCUSDT 3601. 3618. 3530  3583. 22000.    78908771. 175679    11646.       41778836.
 9 2019-01-13 01:00:00 2019-01-14 00:59:59 spot   BTCUSDT 3584. 3611. 3441. 3477. 26386.    92882249. 183113    13692.       48229503.
10 2019-01-14 01:00:00 2019-01-15 00:59:59 spot   BTCUSDT 3478. 3672. 3467. 3626. 35235.   125631820. 229452    18416.       65668831.
# 1,647 more rows
```

