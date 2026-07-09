# Apply a diff object to a dataset

Once changes between \`data.frame\`s have been saved as a \`diff\`
object, they can be used to move between the changes made by applying
the \`diff\` to data.

## Usage

``` r
apply_diff(data, diff, id = "DateTime", invert = FALSE, skip_merge = TRUE)
```

## Arguments

- data:

  the data to apply the \`diff\` to. Must contain all the columns in
  \`diff\`.

- diff:

  a list of \`diff\` or a single \`diff\` objects generated using
  \`get_diff\`.

- id:

  name of the column name used to match observations between \`olddata\`
  and \`newdata\`.

- invert:

  logical. If \`TRUE\` changes will be reversed.

- skip_merge:

  logical. If \`TRUE\` will skip any \`diff\` with are data merges.

## Value

a \`data.frame\` with the same columns as \`data\` with the changes from
\`diff\` applied. Note that this could increase the number of rows if
diff is a data merge.

## Examples

``` r
data1 <- example_data[1:10,]
data2 <- data1
data2$fDOM_QSU[1:4] <- NA
diff <- get_diff(data1, data2)

#get new data from original
newdata2 <- apply_diff(data1, diff)
all.equal(data2, newdata2)
#> [1] TRUE

#get orginal data from new data
newdata1 <- apply_diff(data2, diff, invert=TRUE)
all.equal(data1, newdata1)
#> [1] TRUE
```
