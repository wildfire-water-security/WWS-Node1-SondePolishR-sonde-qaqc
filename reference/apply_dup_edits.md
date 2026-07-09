# Deals with duplicates in data and documents changes

Given a row from \`identify_dups\` and a \`sondeproj\`, the user can
select which data to keep (or take mean) and the requested data summary
will be performed, the appropriate flags will be added, and a changelog
entry will be made.

## Usage

``` r
apply_dup_edits(proj, dup_row, keep_opt, flag_notes = "")
```

## Arguments

- proj:

  \`sondeproj\` object holding sonde data.

- dup_row:

  Row from the outputs of \`identify_dups\`

- keep_opt:

  Character describing which set of duplicates to keep (identified by
  \`DupNum\`), "use_mean" to take the mean of the values, "remove_both"
  to remove all the duplicated values.

- flag_notes:

  Optional character with additional notes to write to the changelog

## Value

a \`sondeproj\` with the updated data, flags, and changelog

## Examples

``` r
path <- file.path(fs::path_package("extdata", package = "SondePolishR"),
"example-sondeproj-messy.RDS")
proj <- readRDS(path)
proj$duplicates <- identify_dups(proj$data)
flagged <- apply_dup_edits(proj, proj$duplicates[1,], "use_mean")
```
