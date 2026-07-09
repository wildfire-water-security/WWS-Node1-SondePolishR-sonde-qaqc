# Sample Sonde Project

An example of a sonde project, which is a list type object that stores
the data, associated metadata, and tracks changes made to the data.

## Usage

``` r
example_sondeproj
```

## Format

An object of class `sondeproj` with the following elements:

- **meta**: A list of length three:

- **site**: A character with the sonde site name.

- **tz**: A character with the site timezone.

- **coords**: A vector of length two with the latitude and longitude of
  the site.

- **data**: A `data.frame` of the dataset, updated with any changes.

- **flags**: A list of length four:

- **flag_rm**: A `data.frame` with the same measurement columns with
  \_flag appended to the name and the same number of rows. Used to store
  flag values for values that were marked as questionable.

- **flag_rm**: A `data.frame` with the same measurement columns with
  \_flag appended to the name and the same number of rows. Used to store
  flag values for values that were removed.

- **flag_chg**: A `data.frame` with the same measurement columns with
  \_flag appended to the name and the same number of rows. Used to store
  flag values for values that were altered.

- **flag_add**: A `data.frame` with the same measurement columns with
  \_flag appended to the name and the same number of rows. Used to store
  flag values for values that were added (likely through interpolation).

- **precip**: A `data.frame` with precipitation values.

- **fieldform**: A `data.frame` with the field form data. See
  `example_fieldform` for details on the structure.

- **calcheck**: A `data.frame` with the calibration check data. See
  `example_calcheck` for details on the structure.

- **diffs**: A list of `data_diff` which stores the changes made to
  `data`. See `daff` package for more details on this structure.

- **changelog**: A `data.frame` which stores a summary of the changes
  made in each `data_diff`:

- **datetime**: The date and time the change was made.

- **parameter**: The parameter that the change was made to.

- **step**: The name of the processing step the change was made in.

- **nchanged**: The number of points that were changed.

- **note**: Notes added about the changes made.

- **user**: The username of the person who made the change.

- **diff_name**: The name of the associated `data.diff`.
