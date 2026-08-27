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

  A adjustment parameter for the selected method (see details).

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
  required to be marked as bad.

- **rel_change**: Based on the `rollmedian` function from the `zoo`
  package which identifies points based on a relative change between the
  points before and after it. The `k` parameter is used to control the
  number of points to include in the median calculation, the `t`
  parameter is used to control the threshold required to be marked as
  bad.

- **high_var**: Used to determine regions of high variability. Uses
  rolling functions from the `zoo` package to determine the difference
  between the point and it's rolling median, then the median absolute
  deviation (MAD) for these differences are caclulated. If the median
  absolute deviation is greater than `t` times the overall data's mean
  MAD it will be marked as bad.

## Examples

``` r
identify_outliers(example_data, "fDOM_QSU", "hampel")
#>   [1]  1429  1543  2105  2129  2190  2762  2815  4765  4771  5123  5134  5490
#>  [13]  5511  5554  5561  5584  5594  5650  5653  5669  5692  5713  5723  5734
#>  [25]  5736  5738  5745  5782  5795  5803  5832  5862  5878  5899  5938  5982
#>  [37]  5992  5998  6054  6067  6089  6090  6091  6098  6104  6150  6158  6173
#>  [49]  6177  6183  6185  6186  6192  6200  6201  6203  6211  6226  6227  6229
#>  [61]  6234  6243  6253  6256  6266  6273  6276  6292  6315  6318  6346  6347
#>  [73]  6358  6374  6378  6393  6403  6412  6413  6425  6426  6446  6448  6476
#>  [85]  6490  6491  6498  6517  6529  6544  6553  6557  6564  6566  6569  6579
#>  [97]  6607  6635  6650  6657  6669  6675  6678  6687  6688  6689  6703  6706
#> [109]  6734  6741  6749  6761  6775  6784  6787  6790  6795  6799  6824  6829
#> [121]  6833  6865  6894  6974  6981  6985  6986  6987  6997  7003  7004  7009
#> [133]  7012  7021  7025  7029  7032  7049  7050  7055  7059  7067  7068  7080
#> [145]  7081  7095  7106  7108  7115  7133  7139  7141  7142  7145  7146  7148
#> [157]  7156  7179  7187  7189  7196  7200  7208  7222  7238  7245  7250  7251
#> [169]  7255  7262  7264  7267  7272  7303  7313  7314  7319  7341  7346  7351
#> [181]  7362  7365  7366  7400  7406  7412  7423  7433  7475  7482  7520  7521
#> [193]  7522  7523  7538  7539  7559  7571  7575  7588  7590  7595  7612  7614
#> [205]  7623  7624  7639  7640  7651  7675  7679  7686  7688  7693  7724  7733
#> [217]  7735  7825  7846  7852  7919  7928  7948  7951  7953  7957  7965  7972
#> [229]  7978  8015  8020  8029  8037  8055  8063  8065 10472 12814 13691 14522
```
