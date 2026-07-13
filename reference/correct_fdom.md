# Apply fDOM temperature and turbidity corrections

Takes functions and coefficient values and applies them to fDOM data
within a sonde project to apply fDOM corrections. Uses flags stored
within the project to only correct data that hasn't previously been
corrected. Additionally, turbidity corrections will only be applied to
temperature corrected data.

## Usage

``` r
correct_fdom(proj, temp = NULL, turb = NULL)
```

## Arguments

- proj:

  A `sondeproj` object.

- temp:

  Either `NULL` or a list object (see details) for temperature
  correction.

- turb:

  Either `NULL` or a list object (see details) for turbidity correction.

## Value

A `data.frame` object with fDOM updated with the corrections.

## Details

To apply a correction you must supply a list object with the following
structure:

- params: a named list with parameter values where the names match the
  arguments within the function

- fun: a function to apply to the fDOM data where the first argument is
  fDOM, the second is either temperature or turbidity, and the third is
  parameters within the function, pulled from `params`.

## Examples

``` r
temp <- list(params = list(rho = -0.011),
             fun = function(fdom, temp, parms){fdom / (1 + parms$rho*(temp - 25))})
corr_data <- correct_fdom(example_sondeproj, temp=temp, turb=NULL)
```
