# Auto-detect potential bad data points

Uses various filtering approaches to attempt to automatically identify
data points that are likely bad and need to be removed.

## Usage

``` r
identify_outliers(data, y_var, method, k = 5, t = 7)
```

## Arguments

- data:

  A `data.frame` with the data to smooth (must have the columns Index
  and y_var)

- y_var:

  Character specifying the variable to apply smoothing to.

- method:

  Character specifying the method to use for smoothing. Options include
  "hampel","rel_change", "high_var". See details for more information
  about the different methods.

- k:

  A adjustment parameter for the selected method (see details). This is
  the total length of points before and after the point.

- t:

  A adjustment parameter for the selected method (see details).

## Value

The indices of points within data flagged as "bad".

## Details

The following detection methods are currently supported:

- **hampel**: Based on the `hampel` function from the `pracma` package
  which identifies points based on a median absolute deviation. The `k`
  parameter is used to control the number of points to include in the
  median calculation, the `t` parameter is used to control the threshold
  required to be marked as bad. The default function was modified to
  ignore points where the MAD is 0.

- **rel_change**: Based on the `rollmedian` function from the `zoo`
  package which identifies points based on a relative change between the
  points before and after it. The `k` parameter is used to control the
  number of points to include in the median calculation, the `t`
  parameter is used to control the threshold required to be marked as
  bad.

- **high_var**: Used to determine regions of high variability. Uses
  rolling functions from the `zoo` package to determine the difference
  between the point and it's rolling median, then the median absolute
  deviation (MAD) for these differences are calculated. If the median
  absolute deviation is greater than `t` times the overall data's mean
  MAD it will be marked as bad.

## Examples

``` r
identify_outliers(example_data, "fDOM_QSU", "hampel")
#>   [1]  1429  1519  1543  2105  2129  2190  2815  2898  5094  5123  5386  5477
#>  [13]  5490  5511  5554  5561  5584  5594  5650  5653  5692  5713  5723  5734
#>  [25]  5745  5782  5795  5803  5810  5832  5878  5899  5938  5982  5992  5998
#>  [37]  6054  6067  6098  6104  6158  6173  6177  6186  6192  6211  6226  6229
#>  [49]  6234  6243  6253  6256  6266  6273  6276  6292  6308  6315  6318  6322
#>  [61]  6346  6347  6358  6374  6393  6403  6412  6413  6425  6426  6446  6448
#>  [73]  6476  6490  6491  6498  6517  6544  6553  6557  6564  6566  6569  6579
#>  [85]  6607  6635  6650  6657  6669  6675  6678  6703  6706  6734  6741  6761
#>  [97]  6775  6784  6787  6790  6799  6808  6824  6829  6833  6865  6894  6974
#> [109]  6981  6993  6997  7003  7004  7009  7012  7021  7025  7029  7032  7049
#> [121]  7050  7055  7059  7067  7068  7080  7081  7095  7108  7115  7133  7139
#> [133]  7142  7145  7148  7156  7179  7187  7189  7196  7200  7208  7222  7238
#> [145]  7245  7250  7251  7255  7262  7264  7267  7272  7303  7313  7314  7319
#> [157]  7341  7346  7351  7362  7365  7366  7400  7406  7412  7423  7433  7475
#> [169]  7482  7503  7506  7538  7539  7559  7571  7575  7588  7590  7595  7612
#> [181]  7614  7623  7626  7639  7640  7644  7646  7651  7675  7679  7686  7688
#> [193]  7693  7721  7724  7733  7735  7761  7793  7825  7846  7852  7919  7928
#> [205]  7933  7948  7951  7953  7957  7965  7972  7978  8015  8019  8020  8029
#> [217]  8037  8055  8063  8065 10459 10461 10472 13479 13632 13679 13717 13794
#> [229] 14044 14140 14141 14522
```
