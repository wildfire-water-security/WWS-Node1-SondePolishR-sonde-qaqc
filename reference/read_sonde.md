# Upload raw sonde data

Loads a `.csv` file with raw sonde data. Turns into a `sonde` object.
Cleans column names, adds a `DateTime` column, and optionally adds a
`flag` column.

## Usage

``` r
read_sonde(
  file,
  return = "df",
  encoding = NULL,
  flags = FALSE,
  skip = NULL,
  tz = "Etc/GMT+8"
)
```

## Arguments

- file:

  File path to sonde data (.csv).

- return:

  What format should data be returned in? Either `df` or `list`.

- encoding:

  File encoding, will guess if `NULL`.

- flags:

  Logical, if `TRUE` a flag column will be added for each parameter.

- skip:

  The number of rows to skip, assumes the first row is a header, will
  guess if `NULL`.

- tz:

  Time zone for the file. If not specified will use user timezone. See
  details for more information.

## Value

If `return` is `df`: **A `data.frame` containing the date, time, site
name, and the "core" measurements (when available):**

- Specific conductivity measured in µS/cm.

- Fluorescent dissolved organic matter (fDOM) measured in Quinine
  Sulfate Units (QSU).

- Dissolved oxygen measured in mg/L.

- Turbidity measured in Formazin Nephelometric Units (FNU).

- pH measured in pH units.

- Temperature measured in degrees C.

- Battery voltage measured in volts.

If `return` is `list`: **A list containing:**

- **serials**: the probe serial numbers associated with each
  measurement.

- **data**: the sonde data formatted as described above.

## Details

Daylights saving time can cause issues for continuous datasets, so data
is often collected in standard time. If this is the case use Etc/GMT
offsets. See [OlsonNames](https://rdrr.io/r/base/timezones.html) for all
available timezones. Note that Etc/GMT signs are **reverse** of UTC
signs. For example UTC-8 would be Etc/GMT+8, signifying Pacific Standard
Time (PST).

## Examples

``` r
file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "example-csv-data1.csv")
data <- read_sonde(file)
#> Error in utf8::as_utf8(text): entry 9 has wrong Encoding; marked as "UTF-8" but invalid leading byte (0xB5) at position 68

file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "sonde-usb-example.csv")
data <- read_sonde(file, return = "list")
#> Error in utf8::as_utf8(text): entry 1 has wrong Encoding; marked as "UTF-8" but invalid leading byte (0xFF) at position 1
```
