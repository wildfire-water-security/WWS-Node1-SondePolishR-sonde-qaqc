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
#>   [1]  1429  1519  1543  2106  2130  2191  2816  2899  5095  5124  5387  5478
#>  [13]  5491  5512  5555  5562  5585  5595  5651  5654  5693  5714  5724  5735
#>  [25]  5746  5783  5796  5804  5811  5833  5879  5900  5939  5983  5993  5999
#>  [37]  6055  6068  6099  6105  6159  6174  6178  6187  6193  6212  6227  6230
#>  [49]  6235  6244  6254  6257  6267  6274  6277  6293  6309  6316  6319  6323
#>  [61]  6347  6348  6359  6375  6394  6404  6413  6414  6426  6427  6447  6449
#>  [73]  6477  6491  6492  6499  6518  6545  6554  6558  6565  6567  6570  6580
#>  [85]  6608  6636  6651  6658  6670  6676  6679  6704  6707  6735  6742  6762
#>  [97]  6776  6785  6788  6791  6800  6809  6825  6830  6834  6866  6895  6975
#> [109]  6982  6994  6998  7004  7005  7010  7013  7022  7026  7030  7033  7050
#> [121]  7051  7056  7060  7068  7069  7081  7082  7096  7109  7116  7134  7140
#> [133]  7143  7146  7149  7157  7180  7188  7190  7197  7201  7209  7223  7239
#> [145]  7246  7251  7252  7256  7263  7265  7268  7273  7304  7314  7315  7320
#> [157]  7342  7347  7352  7363  7366  7367  7401  7407  7413  7424  7434  7476
#> [169]  7483  7504  7507  7539  7540  7560  7572  7576  7589  7591  7596  7613
#> [181]  7615  7624  7627  7640  7641  7645  7647  7652  7676  7680  7687  7689
#> [193]  7694  7722  7725  7734  7736  7762  7794  7826  7847  7853  7920  7929
#> [205]  7934  7949  7952  7954  7958  7966  7973  7979  8016  8020  8021  8030
#> [217]  8038  8056  8064  8066 10462 10464 10475 13482 13635 13682 13720 13797
#> [229] 14047 14143 14144 14532
```
